{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Parser(parseAutomaton) where

import Prelude hiding (lookup)
import Data.Map hiding (foldl, map, filter)
import Data.Aeson
import Data.String
import Data.Maybe
import Data.List hiding (insert, lookup)
import Data.Function
import Automaton


data TransitionJSON = TransitionJSON
    { jsonTransLetter :: String
    , jsonTransFrom   :: String
    , jsonTransTo     :: String 
    } 

data AutomatonJSON = AutomatonJSON 
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

instance FromJSON AutomatonJSON where
    parseJSON = withObject "automaton" $ \o -> do
        jsonStates      <- o .: "states"
        jsonInitial     <- o .: "initial"
        jsonAccepting   <- o .: "accepting"
        jsonTransitions <- o .: "transitions"
        return AutomatonJSON{..}

parseAutomaton :: String -> Maybe Automaton
parseAutomaton str = case decode $ fromString str of
    Nothing -> Nothing
    Just a -> jsonToAutomaton a $ (fst . mapStates . jsonStates) a

mapStates :: [String] -> (Map String Int, Int)
mapStates = (flip foldl) (empty, 0) $ \acc s ->
    (insert s (snd acc) (fst acc), (snd acc) + 1)

jsonToAutomaton :: AutomatonJSON -> Map String Int -> Maybe Automaton
jsonToAutomaton a stMap = 
    case initial' of
        Just inits -> Just $ Automaton transitions' accepting' inits
        _ -> Nothing
    where
        lookupState :: String -> Maybe Int
        lookupState stName = lookup stName stMap
    
        accepting' = catMaybes $ map lookupState acc
            where
                acc = jsonAccepting a
    
        initial' = lookupState $ jsonInitial a

        transitions' = fromList . catMaybes $ map convert ts
            where
                convert = convertJSONTransitions stMap
                ts = groupBy ((==) `on` jsonTransFrom) $ jsonTransitions a

convertJSONTransitions :: Map String Int 
                       -> [TransitionJSON]
                       -> Maybe (Int, Char -> Int)
convertJSONTransitions states transitions = 
    case (from, transitionFunction) of 
        (Just from, _) -> Just (from, transitionFunction)
        _ -> Nothing
    where
        from = lookup (jsonTransFrom $ transitions !! 0) states

        assocs = 
            map (\t -> (letter t, lookup (jsonTransTo t) states)) transitions
            where
                letter :: TransitionJSON -> Char
                letter t = jsonTransLetter t !! 0

        transitionFunction :: Char -> Int
        transitionFunction c = case lookup c directions of
            Nothing -> error "Invalid transition function!" -- impossible!
            Just dir -> dir
            where
                directions = fromList assocs'

                assocs' = map unwrap $ filter isSomething assocs
                    where
                        isSomething :: (Char, Maybe Int) -> Bool
                        isSomething (_, Nothing) = False
                        isSomething _ = True

                        unwrap :: (Char, Maybe Int) -> (Char, Int)
                        unwrap (c, Just i) = (c, i)
                        unwrap _ = error "Invalid state in transition!" --impossible