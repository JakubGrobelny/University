module Automaton (Automaton(..), runAutomaton) where

import System.IO (isEOF)
import Data.Map as Map


data Automaton = Automaton
    { transitions :: Map Int (Char -> Int)
    , accepting   :: [Int]
    , initial     :: Int
    }

runAutomaton :: Automaton -> IO ()
runAutomaton automaton = run initialState
    where
        initialState = initial automaton

        run :: Int -> IO ()
        run state = do
            eof <- isEOF
            case eof of
                False -> do
                    c <- getChar
                    case c of
                        '\r' -> run state --skipping CRLF
                        '\n' -> putStrLn $ yesNo state
                        letter -> run $ updateAutomaton automaton state letter
                True -> putStrLn $ yesNo state

        yesNo :: Int -> String
        yesNo state =
            case elem state $ accepting automaton of
                True -> "yes"
                False -> "no"

updateAutomaton :: Automaton -> Int -> Char -> Int
updateAutomaton automaton state letter = 
    case transition of
        Nothing -> -1
        Just t  -> t letter
    where
        transition = Map.lookup state $ transitions automaton