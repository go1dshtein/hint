module Hint.Interpreter.Process (proceed) where

import Hint.Interpreter.Context

proceed :: Context -> String -> Context
proceed context string = context ++ [string]
