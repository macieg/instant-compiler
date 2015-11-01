import LlvmCompiler

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Data.List.Split
import Data.List
import System.Process

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant

import ErrM

type ParseFun a = [Token] -> Err a

run :: ParseFun Program -> String -> IO ()
run p f = do
	s <- readFile f
	let ts = myLexer s in
		case p ts of
			Bad s -> do
				putStrLn "\nParse              Failed...\n"
				putStrLn "Tokens:"
				putStrLn $ show ts
				putStrLn s
				exitFailure
			Ok tree -> do
				let splitted = splitOn "/" f
				let fileName = head $ splitOn "." $ head $ reverse $ splitted
				let path = intercalate "/" $ take (length splitted - 1) splitted
				let sep = if path == "" then "" else "/"
				let file = path ++ sep ++ fileName
				writeFile (file ++ ".ll") $ compileProgram tree
				ph <- runCommand $ "llvm-as " ++ file ++ ".ll"
				waitForProcess ph
				putStrLn "Finished"


main :: IO ()
main = do
	files <- getArgs
	case files of
		[file] -> run pProgram file