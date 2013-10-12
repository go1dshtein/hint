module Hint.Interpreter.Process
    ( proceed
    , (++=)
    , (!=)
    , (++~)
    , (^.)
    ) where

import Hint.Interpreter.Context
import Control.Lens ((^.))

type WithError = Either Error
type Lens a = ((a -> WithError a) -> Context -> WithError Context)

(++=) :: Lens [a] -> [a] -> Context -> WithError Context
lens ++= container = lens (\x -> Right (x ++ container))

(++~) :: Lens [a] -> a -> Context -> WithError Context
lens ++~ value = lens (\x -> Right (x ++ [value]))

(!=) :: Lens a -> a -> Context -> WithError Context
lens != value = lens (\_ -> Right value)

perform :: Context -> WithError Context
perform context = (output != show (length (context^.command))) context

proceedCommand :: String -> Context -> WithError Context
proceedCommand string context =
    (command ++= string) context >>=
    promptString != (context^.environment.promptLarge) >>=
    (\c -> (history ++~ (c^.command)) c) >>=
    perform >>=
    output ++~ '\n' >>=
    command != ""

waitForEnd :: String -> Context -> WithError Context
waitForEnd string context =
    (command ++= init string) context >>=
    output != "" >>=
    promptString != (context^.environment.promptSmall)

proceed ::  String -> Context -> WithError Context
proceed "" = proceedCommand ""
proceed string =
    if last string == '\\'
        then waitForEnd string
        else proceedCommand string

