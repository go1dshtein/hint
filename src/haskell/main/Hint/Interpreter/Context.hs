{-# LANGUAGE TemplateHaskell #-}
module Hint.Interpreter.Context
    ( Command
    , Environment
    , Macros
    , Context(..)
    , Error(..)
    , initial
    , raise
    , command
    , macroses
    , environment
    , history
    , promptString
    , output
    , welcome
    , farewell
    , promptLarge
    , promptSmall
    ) where

import Control.Lens hiding (Context)

type Command = String

data Environment = Environment
    { _promptLarge:: String
    , _promptSmall :: String
    , _welcome :: String
    , _farewell :: String
    } deriving (Show)

type Macros = String

data Context = Context
    { _command :: Command
    , _macroses :: [Macros]
    , _environment :: Environment
    , _history :: [Command]
    , _promptString :: String
    , _output :: String
    } deriving (Show)

data Error = Error
    { message :: String
    , context :: Context
    }

instance Show Error where
    show Error {message = message, context = context} =
        "ERROR: " ++ message ++ "\nCONTEXT: " ++ show context

makeLenses ''Context
makeLenses ''Environment

initial :: Context
initial = Context
    { _command = ""
    , _macroses = []
    , _environment = Environment
        { _promptLarge = ">> "
        , _promptSmall = "> ... "
        , _welcome = "Hello!"
        , _farewell = "Goodbye!"
        }
    , _history = []
    , _promptString = ">> "
    , _output = ""
    }

raise :: String -> Context -> Error
raise string c = Error
    { message = string
    , context = c
    }

