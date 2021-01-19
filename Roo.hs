-----------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Stage 3
--
--                         Wallaby
-- 
--                    Karl Flores 760493
--                   Marco Marasco 834482
--                  Austen McClernon 834063
-----------------------------------------------------------------
-----------------------------------------------------------------
-- This file contains a Haskell program that can generate ASTs
--  and pretty print Roo source code files.
-----------------------------------------------------------------


module Main (main) 
where
import RooParser (ast)
import PrettyRoo (prettyPrint)
import Compiler (compileRoo)
import SymbolTable (getCode)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Parsec.Error 

data Task
  = Parse | Pprint | Compile | Test
    deriving (Eq, Show)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      
      case task of
        Compile
          -> do
            let [filename] = args
            -- Read file name
            input <- readFile filename
            let output = ast input
            case output of
              Right tree -> do
                              putStr $ getCode (compileRoo tree)
              Left   err -> do
                              putStr "Syntax error at "
                              print err
                              exitWith (ExitFailure 3)
        Parse
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (show tree)
                 Left err 
                   -> do 
                        printError err filename
                        exitWith (ExitFailure 1) 
        Pprint
          -> do
               let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (prettyPrint tree)
                 Left err 
                   -> do 
                         printError err filename
                         exitWith (ExitFailure 2) 

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ ["-a", filename]
  = return Parse
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ [filename]
  = return Compile


printError :: ParseError -> String -> IO ()
printError err filename
  = do
      putStrLn "Oh no, you got an error!\n"
      putStr ("Parse error in file " ++ (show filename) ++ " at ") 
      print err
