import           SeqDiff

main :: IO ()
main = do
    print xs
    mapM_ (putStrLn . showDiff) ds
    where
        ds = diffs xs
        xs = [0, 1, 2, 3, 4, 4, 4, 4, 5, 6, 7, 8, 11, 12]
        showDiff d = case diffType d of
            Duplicate -> "D " ++ show d
            Missing -> "M " ++ show d
