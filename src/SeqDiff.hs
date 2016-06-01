module SeqDiff (
    diffs,
    DiffType(..),
    Diff(..)
) where

data DiffType = Duplicate | Missing deriving Eq

data Diff = Diff { diffType :: DiffType, value :: Int, count :: Int } deriving Eq

instance Show Diff where
    show d = "(" ++ show (value d) ++ ", " ++ show (count d) ++ ")"

diffs :: [Int] -> [Diff]
diffs xs = loop xs 0 0 []

loop :: [Int] -> Int -> Int -> [Diff] -> [Diff]
loop xs ev rc ds =
    case xs of
        v:vs | v == pred ev -> loop vs ev (succ rc) ds
        v:vs -> loop vs (succ v) 0 ds''
            where
            ds' = ds ++ [makeDuplicate (pred ev) rc | rc > 0]
            ds'' = ds' ++ [makeMissing ev (v - ev) | v > ev]
        [] -> ds ++ [makeDuplicate (pred ev) rc | rc > 0]

makeDuplicate :: Int -> Int -> Diff
makeDuplicate v c = Diff {diffType = Duplicate, value = v, count = c}

makeMissing :: Int -> Int -> Diff
makeMissing v c = Diff {diffType = Missing, value = v, count = c}
