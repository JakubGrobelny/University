import Control.Monad
import System.Environment
import Text.Read

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No number of heaps specified!"
        a:_ -> case readMaybe a :: Maybe Int of
            Nothing -> putStrLn "Invalid argument!"
            Just n -> 
                let 
                forever $ do
                putStrLn $ show n
