module Main where

import Parser
import ParserBase
import Interpreter
import AST
import System.Environment
import Control.Monad
-- rename monad return for easy use!
mreturn = Control.Monad.return

-- runs a program and performs all printing
run :: String -> IO ()
run filename =
    (parseFile program filename)
    >>= \x -> mreturn (interpret x)
    >>= \outputs -> mapM printVal outputs
    >>= \_ -> mreturn ()


printVal :: Value -> IO ()
printVal (Number num) = putStrLn (show num)
printVal _ = error "cannot print function."


verifyArgs :: [String] -> String
verifyArgs args =
    case (length args) of
        0 -> error "must pass name of program to be run."
        1 -> (head args)
        otherwise -> error "too many arguments."

main :: IO ()
main =
    getArgs
    >>= \args -> mreturn (verifyArgs args)
    >>= run
