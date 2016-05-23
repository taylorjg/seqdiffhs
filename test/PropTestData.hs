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

isValid :: PropTestData -> Bool
isValid ptd =
    and $ map ($ ptd) checks
    where checks = [
            allMissingDiffValuesAreWithinRange,
            allDuplicateDiffValuesAreWithinRange,
            noDuplicateDiffsForTheSameValue,
            noRunsOfMissingValuesOverlap,
            noRunsOfMissingValuesAdjoin,
            noDuplicateDiffsAndMissingDiffsIntersect,
            lastValueInSequenceIsNotMissing]

allDuplicateDiffValuesAreWithinRange ptd =
    and $ map (< len ptd) (allDuplicateValues ptd)

allMissingDiffValuesAreWithinRange ptd =
    and $ map (< len ptd) (allMissingValues ptd)

noDuplicateDiffsForTheSameValue ptd =
    nub vs == vs
    where vs = map (\d -> value d) $ ds ptd

noRunsOfMissingValuesOverlap ptd =
    (nub $ allMissingValues ptd) == allMissingValues ptd

noRunsOfMissingValuesAdjoin ptd =
    not $ any adjoin (ms ptd)
    where
        adjoin d1 = any (\d2 -> value d2 == value d1 + count d1) (others d1)
        others d = filter (/=d) $ ms ptd

noDuplicateDiffsAndMissingDiffsIntersect ptd =
    null $ intersect (allMissingValues ptd) (allDuplicateValues ptd)

lastValueInSequenceIsNotMissing ptd =
    lastValue `notElem` allMissingValues ptd
    where lastValue = (len ptd) - 1

allDuplicateValues ptd = (ds ptd) >>= (\d -> replicate (count d) (value d))

allMissingValues ptd = (ms ptd) >>= (\d -> take (count d) [(value d)..])
