{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import System.Console.CmdArgs

data Arguments = Arguments
    {
        filename :: [String] -- list of filenames or "-" or empty
    } deriving (Show, Data, Typeable)


description :: Arguments
description = Arguments{filename = [] &= args &= typ "FILE"}
    &= summary "hint v0.1.0.0"
    &= program "hint"
    &= helpArg [explicit, name "h", name "help"]


main :: IO ()
main = do
    arguments <- cmdArgs description
    print arguments