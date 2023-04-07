module Main where

import System.Environment
import System.IO

import Latte.ParLatte

import Interpreter (ErrSt, interpret)
import TypeChecker (typeCheck)

run :: String -> IO ()
run s = case pProgram (myLexer s) of
    Left e ->  hPutStrLn stderr $ "Parsing error\n" ++ e
    Right parsed -> case typeCheck parsed of
        Left err -> hPutStrLn stderr $ "Type check error\n" ++ show err
        Right _ -> interpret parsed

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= run
        fs -> mapM_ (\f -> putStrLn ("======== " ++ f ++ " =======") >> readFile f >>= run) fs
