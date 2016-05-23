module ArbitraryPropTestData () where

import Test.QuickCheck
import PropTestData
import SeqDiff
import Data.List (sortOn)

genPropTestData :: Gen PropTestData
genPropTestData = suchThat gen isValid
    where
    gen = do
        len <- genSequenceLength
        ds <- genDuplicateDiffs len
        ms <- genMissingDiffs len
        return PropTestData {len = len, ds = ds, ms = ms}

genSequenceLength :: Gen Int
genSequenceLength = choose (0, 100)

genDuplicateDiffs :: Int -> Gen [Diff]
genDuplicateDiffs = genDiffs Duplicate

genMissingDiffs :: Int -> Gen [Diff]
genMissingDiffs = genDiffs Missing

genDiffs :: DiffType -> Int -> Gen [Diff]
genDiffs dt n = do
    let tenthOfN = floor $ (realToFrac n) / 10
    numValues <- choose (0, tenthOfN)
    let lastValue = if (n > 0) then n - 1 else 0
    values <- vectorOf numValues $ choose (0, lastValue)
    counts <- vectorOf numValues $ choose (1, 5)
    let vcs = zip values counts
    let ds = map (\(v, c) -> Diff {diffType = dt, value = v, count = c}) vcs
    let ds' = sortOn (\d -> (value d)) ds
    return ds'

shrinkPropTestData :: PropTestData -> [PropTestData]
shrinkPropTestData ptd = []

instance Arbitrary PropTestData where
    arbitrary = genPropTestData
    shrink = shrinkPropTestData
