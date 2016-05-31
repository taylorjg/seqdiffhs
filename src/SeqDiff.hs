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
loop xs ev rc acc =
    case xs of
        v:vs | v == pred ev ->
            loop vs ev (succ rc) acc
        v:vs ->
            let
                acc' = if rc > 0 then makeDuplicate (pred ev) rc:acc else acc
                acc'' = if v > ev then makeMissing ev (v - ev):acc' else acc'
            in
                loop vs (succ v) 0 acc''
        [] ->
            let
                acc' = if rc > 0 then (makeDuplicate (pred ev) rc):acc else acc
            in
                reverse acc'

makeDuplicate :: Int -> Int -> Diff
makeDuplicate v c = Diff {diffType = Duplicate, value = v, count = c}

makeMissing :: Int -> Int -> Diff
makeMissing v c = Diff {diffType = Missing, value = v, count = c}
