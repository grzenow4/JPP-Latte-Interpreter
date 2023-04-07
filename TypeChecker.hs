module TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import System.IO

import Latte.AbsLatte

import Interpreter (ErrSt, Pos, Var, newErr, takeStr, itemStr)

data TType = TInt | TStr | TBool | TTup [TType] | TFun TType [Par] Block deriving Eq

type Env = Map.Map Var TType
data TCSt = TCSt {
    env :: Env,
    whileCnt :: Int,
    ret :: Maybe TType
}

type TCM a = ExceptT ErrSt (State TCSt) a

instance Show TType where
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show (TTup ts) = show ts
    show (TFun _ _ _) = "function"

initState :: TCSt
initState = TCSt { env = Map.empty, whileCnt = 0, ret = Nothing }

takeType :: Type -> TType
takeType (Int _) = TInt
takeType (Str _) = TStr
takeType (Bool _) = TBool
takeType (Tuple _ ts) = TTup $ map takeType ts

writeType :: Var -> TType -> TCM ()
writeType x t = modify (\s -> s { env = Map.insert x t (env s) })

getType :: Pos -> Var -> TCM TType
getType pos x = do
    env <- gets env
    case Map.lookup x env of
        Just t -> return t
        Nothing -> throwError $ newErr pos ("Variable " ++ x ++ " not in scope")

assertInt :: Pos -> TType -> TCM ()
assertInt pos t = case t of
    TInt -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type int with actual type " ++ show t)

assertStr :: Pos -> TType -> TCM ()
assertStr pos t = case t of
    TStr -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type string with actual type " ++ show t)

assertBool :: Pos -> TType -> TCM ()
assertBool pos t = case t of
    TBool -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type bool with actual type " ++ show t)

assertTup :: Pos -> TType -> TCM ()
assertTup pos t = case t of
    TTup _ -> return ()
    _ -> throwError $ newErr pos ("Could not match expected type tuple with actual type " ++ show t)

assertSame :: Pos -> TType -> TType -> TCM ()
assertSame pos t1 t2 = case t1 == t2 of
    True -> return ()
    False -> throwError $ newErr pos ("Could not match expected type " ++ show t1 ++ " with actual type " ++ show t2)

assertParArg :: Par -> Arg -> TCM ()
assertParArg par arg = case (par, arg) of
    (ParVal _ ty _, ValArg pos e) -> checkExp e >>= assertSame pos (takeType ty)
    (ParRef _ ty x, RefArg pos y) -> getType pos (takeStr y) >>= assertSame pos (takeType ty)
    (ParVal _ _ _, RefArg pos _) -> throwError $ newErr pos "Expected a value, but reference passed"
    (ParRef _ _ _, ValArg pos _) -> throwError $ newErr pos "Expected a reference, but value passed"

checkPrg :: [Stmt] -> TCM ()
checkPrg ss = mapM_ checkStmt ss

checkBlock :: Block -> TCM ()
checkBlock (Blck _ ss) = mapM_ checkStmt ss

checkStmt :: Stmt -> TCM ()
checkStmt (Empty _) = return ()
checkStmt (Exp _ e) = checkExp e >> return ()
checkStmt (Ass _ [] e) = error "Impossible case"
checkStmt (Ass pos (x:[]) e) = do
    t <- getType pos (takeStr x)
    checkExp e >>= assertSame pos t
checkStmt (Ass pos xs e) = do
    t <- checkExp e
    case t of
        TTup types -> case length xs == length types of
            True -> mapM_ (\(x,ty) -> getType pos (takeStr x) >>= assertSame pos ty) (zip xs types)
            False -> throwError $ newErr pos "Wrong number of values to unpack"
        ty -> throwError $ newErr pos ("Could not match expected type tuple with actual type " ++ show ty)
checkStmt (AssAdd pos x e) = getType pos (takeStr x) >>= assertInt pos >> checkExp e >>= assertInt pos
checkStmt (AssSub pos x e) = getType pos (takeStr x) >>= assertInt pos >> checkExp e >>= assertInt pos
checkStmt (AssMul pos x e) = getType pos (takeStr x) >>= assertInt pos >> checkExp e >>= assertInt pos
checkStmt (AssDiv pos x e) = getType pos (takeStr x) >>= assertInt pos >> checkExp e >>= assertInt pos
checkStmt (AssMod pos x e) = getType pos (takeStr x) >>= assertInt pos >> checkExp e >>= assertInt pos
checkStmt (Incr pos x) = getType pos (takeStr x) >>= assertInt pos
checkStmt (Decr pos x) = getType pos (takeStr x) >>= assertInt pos
checkStmt (Ret pos e) = do
    ret <- gets ret
    case ret of
        Nothing -> throwError $ newErr pos "Invalid use of return"
        Just t -> checkExp e >>= assertSame pos t
checkStmt (Print _ e) = checkExp e >> return ()
checkStmt (PrintLn _ e) = checkExp e >> return ()
checkStmt (Assert pos e) = checkExp e >>= assertBool pos
checkStmt (If pos e s) = do
    tcs <- get
    checkExp e >>= assertBool pos
    checkBlock s >> put tcs
checkStmt (IfElse pos e s1 s2) = do
    tcs <- get
    checkExp e >>= assertBool pos
    checkBlock s1 >> put tcs >> checkBlock s2 >> put tcs
checkStmt (While pos e s) = do
    tcs <- get
    checkExp e >>= assertBool pos
    modify (\st -> st { whileCnt = (whileCnt st) + 1 })
    checkBlock s >> put tcs
checkStmt (Brk pos) = do
    n <- gets whileCnt
    case n of
        0 -> throwError $ newErr pos "Invalid use of break"
        _ -> return ()
checkStmt (Cntn pos) = do
    n <- gets whileCnt
    case n of
        0 -> throwError $ newErr pos "Invalid use of continue"
        _ -> return ()
checkStmt (BStmt _ ss) = checkBlock ss
checkStmt (Decl _ t its) = mapM_ (\it -> do
    let x = itemStr it
    let ty = takeType t
    case it of
        NoInit _ _ -> writeType x ty
        Init pos _ e -> checkExp e >>= assertSame pos ty >> writeType x ty
    ) its
checkStmt (FnDef _ t x params ss) = do
    writeType (takeStr x) (TFun (takeType t) params ss)
    tcs <- get
    mapM_ (\par -> case par of
        ParVal _ ty y -> writeType (takeStr y) (takeType ty)
        ParRef _ ty y -> writeType (takeStr y) (takeType ty)
        ) params
    modify (\st -> st { whileCnt = 0, ret = Just (takeType t) })
    checkBlock ss
    put tcs

checkExp :: Expr -> TCM TType
checkExp (EOr pos e1 e2) = do
    checkExp e1 >>= assertBool pos
    checkExp e2 >>= assertBool pos
    return TBool
checkExp (EAnd pos e1 e2) = do
    checkExp e1 >>= assertBool pos
    checkExp e2 >>= assertBool pos
    return TBool
checkExp (ERel pos e1 op e2) = do
    t1 <- checkExp e1
    checkExp e2 >>= assertSame pos t1
    return TBool
checkExp (EAdd pos e1 _ e2) = do
    checkExp e1 >>= assertInt pos
    checkExp e2 >>= assertInt pos
    return TInt
checkExp (EMul pos e1 _ e2) = do
    checkExp e1 >>= assertInt pos
    checkExp e2 >>= assertInt pos
    return TInt
checkExp (Not pos e) = do
    checkExp e >>= assertBool pos
    return TBool
checkExp (Neg pos e) = do
    checkExp e >>= assertInt pos
    return TInt
checkExp (EVar pos x) = getType pos (takeStr x)
checkExp (EInt _ _) = return TInt
checkExp (EString _ _) = return TStr
checkExp (ETrue _) = return TBool
checkExp (EFalse _) = return TBool
checkExp (ETuple _ es) = mapM checkExp es >>= return . TTup
checkExp (EApp pos x args) = do
    t <- getType pos (takeStr x)
    case t of
        TFun ty params _ -> case length params == length args of
            True -> mapM_ (\(par,arg) -> assertParArg par arg) (zip params args) >> return ty
            False -> throwError $ newErr pos "Wrong number of arguments passed to a function"
        _ -> throwError $ newErr pos ((takeStr x) ++ " is not a function")

typeCheck :: Program -> Either ErrSt ()
typeCheck (Prog _ prg) = fst $ runState (runExceptT (checkPrg prg)) initState
