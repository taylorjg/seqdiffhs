module PropTestData (
    PropTestData(..),
    toList,
    allDiffs,
    allDuplicateValues,
    allMissingValues,
    isValid
) where

import SeqDiff
import Data.List (sortOn, (\\))

data PropTestData = PropTestData {
    len :: Int,
    ds :: [Diff],
    ms :: [Diff]
} deriving Show

toList :: PropTestData -> [Int]
toList ptd = xs' >>= addDups
    where
        addDups x =
            let count = length $ filter (==x) (allDuplicateValues ptd)
            in replicate (1 + count) x
        xs' = xs \\ (allMissingValues ptd)
        xs = take (len ptd) [0..]

allDiffs :: PropTestData -> [Diff]
allDiffs ptd = sortOn (\a -> value a) $ concat [(ds ptd), (ms ptd)]

allDuplicateValues :: PropTestData -> [Int]
allDuplicateValues ptd = (ds ptd) >>= (\d -> replicate (count d) (value d))

allMissingValues :: PropTestData -> [Int]
allMissingValues ptd = (ms ptd) >>= (\d -> take (count d) [(value d)..])

isValid :: PropTestData -> Bool
isValid ptd = True
