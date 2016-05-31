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
        v:vs | v == pred ev -> loop vs ev (succ rc) acc
        v:vs ->
            let
                d1 = if rc > 0 then Just $ makeDuplicate (pred ev) rc else Nothing
                d2 = if v > ev then Just $ makeMissing ev (v - ev) else Nothing
                ds = case (d1, d2) of
                    (Just d1, Just d2) -> [d2, d1]
                    (Just d1, Nothing) -> [d1]
                    (Nothing, Just d2) -> [d2]
                    (Nothing, Nothing) -> []
            in
                loop vs (succ v) 0 (ds ++ acc)
        [] ->
            let acc' = if rc > 0 then (makeDuplicate (pred ev) rc):acc else acc
            in reverse acc'

makeDuplicate :: Int -> Int -> Diff
makeDuplicate v c = Diff {diffType = Duplicate, value = v, count = c}

makeMissing :: Int -> Int -> Diff
makeMissing v c = Diff {diffType = Missing, value = v, count = c}
