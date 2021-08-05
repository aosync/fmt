import System.Environment
import Text.Read


main :: IO()
main = do
    args <- getArgs
    contents <- getContents

    let len = case args of
                  [] -> (Just 75)
                  (fst:_) -> readMaybe fst

    case len of
        Nothing -> do
            putStr $ relineBy 75 $ contents
        (Just l) -> do
            putStr $ relineBy l $ contents

relineBy :: Int -> String -> String
relineBy len contents =
    let w = map words . lines $ contents
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
