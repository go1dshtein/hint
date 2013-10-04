{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.List (elem)
import System.IO
    ( hGetContents
    , withFile
    , IOMode(..)
    , isEOF
    , hFlush
    , hPrint
    , stdout
    , stderr)
import Control.Monad (foldM)

import System.Console.CmdArgs
import System.Posix.Signals
import System.Exit

import Hint.Interpreter.Context
import Hint.Interpreter.Process (proceed, (^.))

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
    | null (filename arguments)     = proceedInteractive context
    | elem "-" $ filename arguments = proceedStdIn context
    | otherwise                     = proceedFiles (filename arguments) context
    where context = Right initial


proceedError :: Either Error Context -> IO ()
proceedError (Left error) = do
    hPrint stderr error
    exitFailure


proceedInteractive :: Either Error Context -> IO ()
proceedInteractive (Left error) = proceedError (Left error)
proceedInteractive (Right context) = do
    installHandler keyboardSignal (Catch (putStrLn "" >> prompt)) Nothing
    putStrLn $ context^.environment.welcome
    proceedInteractive' (Right context)
    where
        proceedInteractive' (Left error) = proceedError (Left error)
        proceedInteractive' (Right context) = do
            putStr $ context^.output
            putStr $ context^.promptString
            hFlush stdout
            eof <- isEOF
            if eof
                then do
                    putStrLn $ context^.environment.farewell
                    exitSuccess
                else do
                    line <- getLine
                    proceedInteractive' $ proceed line context
        prompt = putStr (context^.environment.promptLarge) >> hFlush stdout


proceedStdIn :: Either Error Context -> IO ()
proceedStdIn (Left error) = proceedError (Left error)
proceedStdIn (Right context) = do
    input <- getContents
    result $ foldM (flip proceed) context $ lines input
    where
        result (Left error) = proceedError (Left error)
        result (Right context) = do
            putStr $ context^.output
            exitSuccess


proceedFiles ::  [String] -> Either Error Context -> IO ()
proceedFiles _ (Left error) = proceedError (Left error)
proceedFiles [] (Right context) = do
    putStr $ context^.output
    exitSuccess
proceedFiles (filename:filenames) (Right context) =
    withFile filename ReadMode $ \handler -> do
        file <- hGetContents handler
        proceedFiles filenames $ foldM (flip proceed) context $ lines file


main :: IO ()
main = dispatch =<< cmdArgs description
