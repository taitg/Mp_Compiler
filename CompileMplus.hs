
{-
Main program module for M+ compiler
-}

module Main where

import System.IO
import System.Environment
import System.Exit ( exitFailure, exitSuccess )
import LexMplus
import ParMplus
import SkelMplus
import PrintMplus
import AbsMplus
import IRMplus
import AMCodeMplus
import ErrM
import GenericPretty

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse Failed...\n"
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      --putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: ./TestMplus <input file>" ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  if (not $ length args == 2) then usage
  else case args of
    ["--help"] -> usage
    (f:fs) -> do 
        mapM_ (runFile 2 pProg) [f]
        conts <- readFile (args !! 0)
        out <- openFile (args !! 1) WriteMode
        let toks = myLLexer conts
        let ptree = pProg toks
        case ptree of
            Ok tree -> do
                putStrLn $ unlines
                    [   "\n---------------------"
                        ,"Abstract Syntax Tree:"
                        ,"---------------------" ]
                let ast = transProg tree
                putStr (pretty ast)
                putStrLn $ unlines
                    [   "\n\n----------------------------"
                        ,"Intermediate representation:"
                        ,"----------------------------" ]
                let ir = irProg ast
                putStr (pretty ir)
                putStrLn "\n"
                case ir of
                    Ok irtree -> do
                        let am = codeProg irtree 0
                        hPutStr out am
                    otherwise -> error "Exception: semantic analysis failure"
            otherwise -> error "Exception: parse failure"
        hClose out





