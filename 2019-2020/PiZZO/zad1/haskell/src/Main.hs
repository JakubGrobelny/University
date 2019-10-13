module Main where

import System.IO (isEOF)
import Control.Monad.Loops
import Parser
import Automaton


main :: IO ()
main = do
    filePath <- getLine
    automatonJSON <- readFile $ fixCRLF filePath
    case parseAutomaton automatonJSON of
        Nothing -> error "Invalid automaton description!"
        Just automaton -> do
            untilM_ (runAutomaton automaton) isEOF
    where
        fixCRLF :: String -> String
        fixCRLF = filter (/= '\r')