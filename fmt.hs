import System.Environment
import Text.Read
import Data.List
import Data.Function

main :: IO()
main = do
    args <- getArgs
    contents <- getContents

    putStr . relineBy (case (headMaybe args >>= readMaybe) of
                          (Just x) -> x
                          Nothing -> 75)
                      $ contents

relineBy :: Int -> String -> String
relineBy len contents =
    let w = map words . lines $ noret contents
        rel = go [] w len
    in unlines . map unwords $ rel
    where
        go :: [String] -> [[String]] -> Int -> [[String]]
        go cur [] _ = [cur]
        go cur ([]:r) len = cur : go [] r len
        go cur ((x:xs):r) len
            | ( length . unwords $ x' ) > len = cur : go [x] (xs:r) len
            | otherwise = go x' (xs:r) len
            where x' = cur ++ [x]
        noret :: String -> String
        noret [] = ""
        noret ('\n':'\n':xs) = '\n' : '\n' : noret(xs)
        noret ('\n':xs) = ' ' : noret xs
        noret (x:xs) = x : noret xs

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x
