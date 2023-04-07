module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import System.IO

import Latte.AbsLatte

type Var = String
type Loc = Int
data Value = VInt Integer | VStr String | VBool Bool | VTup [Value] deriving (Eq, Ord)
type Fun = [Arg] -> IM Value

type Env = Map.Map Var Loc
type Store = Map.Map Loc Value
type FStore = Map.Map Loc Fun
data St = St {
    env :: Env,
    store :: Store,
    funcs :: FStore
}

type Pos = BNFC'Position
data ErrSt = ErrSt {
    pos :: Pos,
    reason :: String
}

type IM a = ExceptT ErrSt (StateT St IO) a
data Ret = RVal Value | RVoid | RBreak | RContinue

instance Show Value where
    show v = case v of
        VInt n -> show n
        VStr s -> show s
        VBool b -> if b then "true" else "false"
        VTup xs -> show xs

instance Show ErrSt where
    show err = case (pos err, reason err) of
        (Just (row, col), s) -> "(" ++ show row ++ ":" ++ show col ++ "): " ++ s
        (_, s) -> "(_:_): " ++ s

initState :: St
initState = St { env = Map.empty, store = Map.empty, funcs = Map.empty }

defInt, defStr, defBool :: Value
defInt = VInt 0
defStr = VStr ""
defBool = VBool False

defTup :: [Type] -> Value
defTup = VTup . map (\t -> case t of
    Int _ -> defInt
    Str _ -> defStr
    Bool _ -> defBool
    Tuple _ ts -> defTup ts
    )

aop :: AddOp -> Integer -> Integer -> Integer
aop (Plus _) = (+)
aop (Minus _) = (-)

mop :: MulOp -> Integer -> Integer -> Integer
mop (Times _) = (*)
mop (Div _) = div
mop (Mod _) = mod

rop :: Ord a => RelOp -> a -> a -> Bool
rop (LTH _) = (<)
rop (LE _) = (<=)
rop (GTH _) = (>)
rop (GE _) = (>=)
rop (EQU _) = (==)
rop (NE _) = (/=)

takeStr :: Ident -> String
takeStr (Ident x) = x

itemStr :: Item -> String
itemStr (NoInit _ x) = takeStr x
itemStr (Init _ x _) = takeStr x

newErr :: Pos -> String -> ErrSt
newErr pos s = ErrSt { pos = pos, reason = s }

newloc :: IM Loc
newloc = do
    s <- get
    return $ Map.size (store s) + Map.size (funcs s)

changeEnv :: Env -> IM ()
changeEnv e = modify (\s -> s { env = e })

writeLoc :: Var -> Loc -> IM ()
writeLoc x l = modify (\s -> s { env = Map.insert x l (env s) })

writeVal :: Loc -> Value -> IM ()
writeVal l v = modify (\s -> s { store = Map.insert l v (store s) })

writeFun :: Loc -> Fun -> IM ()
writeFun l f = modify (\s -> s { funcs = Map.insert l f (funcs s) })

getLoc :: Var -> IM Loc
getLoc x = do
    env <- gets env
    case Map.lookup x env of
        Just l -> return l
        Nothing -> error "TypeChecker failed"

getVal :: Loc -> IM Value
getVal l = do
    store <- gets store
    case Map.lookup l store of
        Just v -> return v
        Nothing -> error "Run out of memory"

getFun :: Loc -> IM Fun
getFun l = do
    funcs <- gets funcs
    case Map.lookup l funcs of
        Just f -> return f
        Nothing -> error "Run out of memory"

writeArg :: Env -> (Par, Arg) -> IM ()
writeArg env1 (par, arg) = case (par, arg) of
    (ParVal _ _ x, ValArg _ e) -> do
        env0 <- gets env
        changeEnv env1
        v <- evalExp e
        changeEnv env0
        l <- newloc
        writeLoc (takeStr x) l
        writeVal l v
    (ParRef _ _ x, RefArg _ y) -> do
        let (Just l) = Map.lookup (takeStr y) env1
        writeLoc (takeStr x) l
    (_, _) -> error "TypeChecker failed"

execPrg :: [Stmt] -> IM Ret
execPrg ss = mapM_ exec ss >> return RVoid

execBlck :: Block -> IM Ret
execBlck (Blck _ ss) = do
    env <- gets env
    res <- helper ss
    changeEnv env
    return res where
        helper :: [Stmt] -> IM Ret
        helper [] = return RVoid
        helper (s:ss) = do
            r <- exec s
            case r of
                RVal v -> return $ RVal v
                RVoid -> helper ss
                r -> return r

exec :: Stmt -> IM Ret
exec (Empty _) = return RVoid
exec (Exp _ e) = evalExp e >> return RVoid
exec (Ass _ [] e) = error "Impossible case"
exec (Ass _ (x:[]) e) = do
    l <- getLoc (takeStr x)
    v <- evalExp e
    writeVal l v
    return RVoid
exec (Ass _ xs e) = do
    v <- evalExp e
    case v of
        VTup ys -> mapM_ (\(x, y) -> getLoc (takeStr x) >>= (flip writeVal) y) (zip xs ys)
        _ -> error "TypeChecker failed"
    return RVoid
exec (AssAdd pos x e) = do
    v1 <- evalExp (EVar pos x)
    v2 <- evalExp e
    l <- getLoc (takeStr x)
    case (v1, v2) of
        (VInt i1, VInt i2) -> writeVal l (VInt $ i1 + i2)
        (_, _) -> error "TypeChecker failed"
    return RVoid
exec (AssSub pos x e) = do
    v1 <- evalExp (EVar pos x)
    v2 <- evalExp e
    l <- getLoc (takeStr x)
    case (v1, v2) of
        (VInt i1, VInt i2) -> writeVal l (VInt $ i1 - i2)
        (_, _) -> error "TypeChecker failed"
    return RVoid
exec (AssMul pos x e) = do
    v1 <- evalExp (EVar pos x)
    v2 <- evalExp e
    l <- getLoc (takeStr x)
    case (v1, v2) of
        (VInt i1, VInt i2) -> writeVal l (VInt $ i1 * i2)
        (_, _) -> error "TypeChecker failed"
    return RVoid
exec (AssDiv pos x e) = do
    v1 <- evalExp (EVar pos x)
    v2 <- evalExp e
    l <- getLoc (takeStr x)
    case (v1, v2) of
        (_, VInt 0) -> throwError $ newErr pos "Cannot divide by 0"
        (VInt i1, VInt i2) -> writeVal l (VInt $ div i1 i2)
        (_, _) -> error "TypeChecker failed"
    return RVoid
exec (AssMod pos x e) = do
    v1 <- evalExp (EVar pos x)
    v2 <- evalExp e
    l <- getLoc (takeStr x)
    case (v1, v2) of
        (_, VInt 0) -> throwError $ newErr pos "Cannot modulo by 0"
        (VInt i1, VInt i2) -> writeVal l (VInt $ mod i1 i2)
        (_, _) -> error "TypeChecker failed"
    return RVoid
exec (Incr _ x) = do
    l <- getLoc (takeStr x)
    v <- getVal l
    case v of
        VInt i -> writeVal l (VInt (i + 1))
        _ -> error "TypeChecker failed"
    return RVoid
exec (Decr _ x) = do
    l <- getLoc (takeStr x)
    v <- getVal l
    case v of
        VInt i -> writeVal l (VInt (i - 1))
        _ -> error "TypeChecker failed"
    return RVoid
exec (Ret _ e) = evalExp e >>= return . RVal
exec (Print _ e) = evalExp e >>= liftIO . putStr . show >> return RVoid
exec (PrintLn _ e) = evalExp e >>= liftIO . putStrLn . show >> return RVoid
exec (Assert pos e) = do
    v <- evalExp e
    case v of
        (VBool b) -> if b then return RVoid else throwError $ newErr pos "Assert failed"
        _ -> error "TypeChecker failed"
exec (If pos e s) = do
    v <- evalExp e
    case v of
        VBool b -> if b then execBlck s else return RVoid
        _ -> error "TypeChecker failed"
exec (IfElse _ e s1 s2) = do
    v <- evalExp e
    case v of
        VBool b -> if b then execBlck s1 else execBlck s2
        _ -> error "TypeChecker failed"
exec (While pos e s) = do
    v <- evalExp e
    case v of
        VBool b -> if b then do
            res <- execBlck s
            case res of
                RVal _ -> return RVoid
                RVoid -> exec (While pos e s)
                RBreak -> return RVoid
                RContinue -> exec (While pos e s)
        else return RVoid
        _ -> error "TypeChecker failed"
exec (Brk _) = return RBreak
exec (Cntn _) = return RContinue
exec (BStmt _ ss) = execBlck ss
exec (Decl _ ty its) = mapM_ (\it -> do
    l <- newloc
    writeLoc (itemStr it) l
    case (ty, it) of
        (Int _, NoInit _ x) -> writeVal l defInt
        (Str _, NoInit _ x) -> writeVal l defStr
        (Bool _, NoInit _ x) -> writeVal l defBool
        (Tuple _ ts, NoInit _ x) -> writeVal l (defTup ts)
        (_, Init _ x e) -> evalExp e >>= writeVal l
    ) its >> return RVoid
exec (FnDef pos ty x params ss) = do
    env0 <- gets env
    l <- newloc
    writeLoc (takeStr x) l
    writeFun l (\args -> do
        env1 <- gets env
        changeEnv env0
        mapM_ (writeArg env1) (zip params args)
        writeLoc (takeStr x) l
        res <- execBlck ss
        changeEnv env1
        case res of
            RVal v -> return v
            RVoid -> case ty of
                Int _ -> return defInt
                Str _ -> return defStr
                Bool _ -> return defBool
                Tuple _ ts -> return $ defTup ts
            _ -> throwError $ newErr pos "Invalid return from the function"
        )
    return RVoid

evalExp :: Expr -> IM Value
evalExp (EOr _ e1 e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return . VBool $ b1 || b2
        (_, _) -> error "TypeChecker failed"
evalExp (EAnd _ e1 e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return . VBool $ b1 && b2
        (_, _) -> error "TypeChecker failed"
evalExp (ERel _ e1 op e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return . VBool $ rop op i1 i2
        (VStr s1, VStr s2) -> return . VBool $ rop op s1 s2
        (VBool b1, VBool b2) -> return . VBool $ rop op b1 b2
        (VTup t1, VTup t2) -> return . VBool $ rop op t1 t2
        (_, _) -> error "TypeChecker failed"
evalExp (EAdd _ e1 op e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return . VInt $ aop op i1 i2
        (_, _) -> error "TypeChecker failed"
evalExp (EMul pos e1 op e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2, op) of
        (_, VInt 0, Div _) -> throwError $ newErr pos "Cannot divide by 0"
        (_, VInt 0, Mod _) -> throwError $ newErr pos "Cannot modulo by 0"
        (VInt i1, VInt i2, _) -> return . VInt $ mop op i1 i2
        (_, _, _) -> error "TypeChecker failed"
evalExp (Not _ e) = do
    v <- evalExp e
    case v of
        VBool b -> return . VBool $ not b
        _ -> error "TypeChecker failed"
evalExp (Neg _ e) = do
    v <- evalExp e
    case v of
        VInt i -> return . VInt $ negate i
        _ -> error "TypeChecker failed"
evalExp (EVar _ x) = getLoc (takeStr x) >>= getVal
evalExp (EInt _ n) = return $ VInt n
evalExp (EString _ s) = return $ VStr s
evalExp (ETrue _) = return $ VBool True
evalExp (EFalse _) = return $ VBool False
evalExp (ETuple _ es) = mapM evalExp es >>= return . VTup
evalExp (EApp _ x args) = getLoc (takeStr x) >>= getFun >>= \f -> f args

handleErr :: ErrSt -> IM Ret
handleErr err = do
    liftIO . hPutStrLn stderr $ "Runtime error\n" ++ show err
    return RVoid

interpret :: Program -> IO ()
interpret (Prog _ prg) = do
    execStateT (runExceptT (catchError (execPrg prg) handleErr)) initState
    return ()
