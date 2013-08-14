module Hint.Interpreter.Context (Context(..), getContext) where

type Context = [String]

getContext :: Maybe Context
getContext = Just ["nothing"]
