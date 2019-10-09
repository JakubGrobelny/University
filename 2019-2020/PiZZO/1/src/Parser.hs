{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Parser(parseAutomata) where

import Data.Aeson
import Automata


data TransitionJSON = TransitionJSON
    { jsonTransLetter :: String
    , jsonTransFrom   :: String
    , jsonTransTo     :: String 
    }

data AutomataJSON = AutomataJSON 
    { jsonStates      :: [String]
    , jsonInitial     :: String
    , jsonAccepting   :: [String]
    , jsonTransitions :: [TransitionJSON]
    }

instance FromJSON TransitionJSON where
    parseJSON = withObject "transition" $ \o -> do
        jsonTransLetter <- o .: "letter"
        jsonTransFrom   <- o .: "from"
        jsonTransTo     <- o .: "to"
        return TransitionJSON{..}

instance FromJSON AutomataJSON where
    parseJSON = withObject "automata" $ \o -> do
        jsonStates      <- o .: "states"
        jsonInitial     <- o .: "initial"
        jsonAccepting   <- o .: "accepting"
        jsonTransitions <- o .: "transitions"
        return AutomataJSON{..}

parseAutomata :: String -> Maybe Automata
parseAutomata = undefined
