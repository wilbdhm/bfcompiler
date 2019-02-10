module Main where

import Brainfuck
import Compiler
import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    let fnames = case args of
            [] -> ["main.bf"]
            _  -> args
        outputs = map ((++ ".nasm") . removeSuff) fnames
    contents <- mapM readFile fnames
    let sources = map parseBf contents
    zipWithM_ compileBf sources outputs

removeSuff :: String -> String
removeSuff = takeWhile (/= '.')