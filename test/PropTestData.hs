module PropTestData (
    PropTestData(..),
    allDiffs,
    allDuplicateValues,
    allMissingValues,
    isValid
) where

import SeqDiff
import Data.List (sortOn)

data PropTestData = PropTestData {
    length :: Int,
    ds :: [Diff],
    ms :: [Diff]
} deriving Show

allDiffs :: PropTestData -> [Diff]
allDiffs ptd = sortOn (\a -> value a) $ concat [(ds ptd), (ms ptd)]

allDuplicateValues :: PropTestData -> [Int]
allDuplicateValues ptd = []

allMissingValues :: PropTestData -> [Int]
allMissingValues ptd = []

isValid :: PropTestData -> Bool
isValid ptd = True
