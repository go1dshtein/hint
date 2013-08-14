module Hint.Interpreter.Process (proceed) where

import Hint.Interpreter.Context


proceed :: Context -> String -> Maybe Context
proceed context _ = Just context
