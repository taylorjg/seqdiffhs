import           SeqDiff

main :: IO ()
main = mapM_ putStrLn $ map show ds
    where
        ds = diffs xs
        xs = [0, 1, 2, 3, 4, 4, 4, 4, 5, 6, 7, 8, 11, 12]
