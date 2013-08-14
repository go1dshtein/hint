{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.List (elem)
import System.IO (hGetContents, withFile, IOMode(..), isEOF, hFlush, stdout)

import System.Console.CmdArgs
import System.Posix.Signals

import Hint.Interpreter.Context
import Hint.Interpreter.Process (proceed)

data Arguments = Arguments
    {
        filename :: [String] -- list of filenames or "-" or empty
    } deriving (Show, Data, Typeable)


description :: Arguments
description = Arguments{filename = [] &= args &= typ "FILE"}
    &= summary "hint v0.1.0.0"
    &= program "hint"
    &= helpArg [explicit, name "h", name "help"]


dispatch :: Arguments -> IO ()
dispatch arguments
    | filename arguments == []      = proceedInteractive context
    | elem "-" $ filename arguments = proceedStdIn context
    | otherwise                     = proceedFiles (filename arguments) context
    where context = getContext


proceedInteractive :: Context -> IO ()
proceedInteractive context = do
    installHandler keyboardSignal (Catch (putStrLn "" >> promt)) Nothing
    putStrLn "Hello!"
    proceedInteractive' context
    where
        proceedInteractive' context = do
            promt
            eof <- isEOF
            if eof
                then do
                    print context
                    putStrLn "Goodbye!"
                else do
                    line <- getLine
                    let newcontext = proceed context line
                    print newcontext
                    proceedInteractive' newcontext
        promt = putStr ">> " >> hFlush stdout


proceedStdIn :: Context -> IO ()
proceedStdIn context = do
    input <- getContents
    print $ foldl proceed context $ lines input


proceedFiles ::  [String] -> Context -> IO ()
proceedFiles [] context = print context
proceedFiles (filename:filenames) context =
    withFile filename ReadMode $ \handler -> do
        file <- hGetContents handler
        proceedFiles filenames $ foldl proceed context $ lines file


main :: IO ()
main = dispatch =<< cmdArgs description
