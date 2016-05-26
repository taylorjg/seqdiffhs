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
        diffs <- genDiffs
        let ds = fst diffs
        let ms = snd diffs
        return PropTestData {len = len, ds = ds, ms = ms}

genSequenceLength :: Gen Int
genSequenceLength = choose (0, 100)

genDiffs :: Gen ([Diff], [Diff])
genDiffs = do
    numDiffs <- genNumDiffs
    diffs <- vectorOf numDiffs arbitrary :: Gen [Diff]
    let ds = filter (isDiffType Duplicate) diffs
    let ms = filter (isDiffType Missing) diffs
    return (ds, ms)
    where isDiffType dt d = diffType d == dt

genNumDiffs :: Gen Int
genNumDiffs = frequency [(2, return 0), (98, choose (1, 10))]

shrinkPropTestData :: PropTestData -> [PropTestData]
shrinkPropTestData ptd = concatMap ($ ptd) [
    shrinkDuplicateDiffs,
    shrinkMissingDiffs,
    shrinkSequenceLength
    ]

shrinkDuplicateDiffs ptd =
    filter isValidShrink $ map makePropTestData shrinks
    where
        makePropTestData ds = ptd {ds = ds}
        shrinks = shrink (ds ptd)

shrinkMissingDiffs ptd =
    filter isValidShrink $ map makePropTestData shrinks
    where
        makePropTestData ms = ptd {ms = ms}
        shrinks = shrink (ms ptd)

shrinkSequenceLength ptd =
    take 1 $ filter isValidShrink $ map makePropTestData $ reverse [0..len ptd - 1]
    where makePropTestData len = ptd {len = len}

isValidShrink ptd = isValid ptd && not (null $ allDiffs ptd)

genDiff :: Gen Diff
genDiff = do
    dt <- elements [Duplicate, Missing]
    v <- choose (0, 100)
    c <- choose (1, 5)
    return Diff {diffType = dt, value = v, count = c}

shrinkDiff :: Diff -> [Diff]
shrinkDiff d = [d {count = count d - 1} | count d > 1]

instance Arbitrary PropTestData where
    arbitrary = genPropTestData
    shrink = shrinkPropTestData

instance Arbitrary Diff where
    arbitrary = genDiff
    shrink = shrinkDiff
