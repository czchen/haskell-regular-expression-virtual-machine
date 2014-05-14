import System.Environment
import Text.Read

import Re

main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "program [pattern] [string]"
    else
        let
            pattern = args !! 0
            string = args !! 1
        in
            case Re.match pattern string of
            Nothing -> putStrLn $ "Not support regular express " ++ pattern
            Just True -> putStrLn "Match"
            Just False -> putStrLn "No match"
