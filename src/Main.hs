import           SeqDiff

main :: IO ()
main = putStrLn $ show ds
    where
        xs = [0, 1, 2, 3, 4, 4, 4, 4, 5, 6, 7, 8, 11, 12]
        ds = diffs xs
