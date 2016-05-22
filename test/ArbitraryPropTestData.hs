module ArbitraryPropTestData () where

import Test.QuickCheck
import PropTestData
import SeqDiff
import Data.List (sortOn)
import Data.Bits (shiftR)

genPropTestData :: Gen PropTestData
genPropTestData = do
    len <- genSequenceLength
    ds <- genDuplicateDiffs len
    ms <- genMissingDiffs len
    let ptd = PropTestData {len = len, ds = ds, ms = ms}
    return ptd

genSequenceLength :: Gen Int
genSequenceLength = choose (0, 100)

genDuplicateDiffs :: Int -> Gen [Diff]
genDuplicateDiffs = genDiffs Duplicate

genMissingDiffs :: Int -> Gen [Diff]
genMissingDiffs = genDiffs Missing

genDiffs :: DiffType -> Int -> Gen [Diff]
genDiffs dt n = do
    let lastValue = n - 1
    let tenthOfN = floor $ (realToFrac n) / 10
    numValues <- choose (0, tenthOfN)
    values <- vectorOf numValues $ choose (0, lastValue)
    counts <- vectorOf numValues $ choose (1, 5)
    let vcs = zip values counts
    let ds = map (\(v, c) -> Diff {diffType = dt, value = v, count = c}) vcs
    return $ sortOn (\d -> (value d)) ds

shrinkPropTestData :: PropTestData -> [PropTestData]
shrinkPropTestData ptd = []

instance Arbitrary PropTestData where
    arbitrary = genPropTestData
    shrink = shrinkPropTestData
