module PropTestData (
    PropTestData(..),
    toList,
    allDiffs,
    isValid
) where

import SeqDiff
import Data.List (sortOn, nub, intersect, (\\))

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
isValid ptd =
    and $ map ($ ptd) checks
    where checks = [
            allDuplicateDiffValuesAreWithinRange,
            allMissingDiffValuesAreWithinRange,
            noDuplicateDiffsForTheSameValue,
            noRunsOfMissingValuesOverlap,
            noDuplicateDiffsAndMissingDiffsIntersect,
            lastValueInSequenceIsNotMissing
            ]

allDuplicateDiffValuesAreWithinRange :: PropTestData -> Bool
allDuplicateDiffValuesAreWithinRange ptd =
    and $ map (< len ptd) (allDuplicateValues ptd)

allMissingDiffValuesAreWithinRange :: PropTestData -> Bool
allMissingDiffValuesAreWithinRange ptd =
    and $ map (< len ptd) (allMissingValues ptd)

noDuplicateDiffsForTheSameValue :: PropTestData -> Bool
noDuplicateDiffsForTheSameValue ptd =
    nub vs == vs
    where vs = map (\d -> value d) $ ds ptd

noRunsOfMissingValuesOverlap :: PropTestData -> Bool
noRunsOfMissingValuesOverlap ptd =
    (nub $ allMissingValues ptd) == allMissingValues ptd

noDuplicateDiffsAndMissingDiffsIntersect :: PropTestData -> Bool
noDuplicateDiffsAndMissingDiffsIntersect ptd =
    null $ intersect (allMissingValues ptd) (allDuplicateValues ptd)

lastValueInSequenceIsNotMissing :: PropTestData -> Bool
lastValueInSequenceIsNotMissing ptd =
    lastValue `notElem` allMissingValues ptd
    where lastValue = (len ptd) - 1
