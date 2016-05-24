import           SeqDiff

main :: IO ()
main = do
    putStrLn $ show xs
    mapM_ putStrLn $ map showDiff ds
    where
        ds = diffs xs
        xs = [0, 1, 2, 3, 4, 4, 4, 4, 5, 6, 7, 8, 11, 12]
        showDiff d@Diff {diffType = dt, value = v, count = c} = case dt of
            Duplicate -> "D " ++ show d
            Missing -> "M " ++ show d
