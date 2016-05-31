import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit (exitFailure)
import SeqDiff
import PropTestData
import ArbitraryPropTestData

prop_diffs :: PropTestData -> Property
prop_diffs ptd =
    classify (and [null $ ds ptd, null $ ms ptd]) "No diffs" $
    classify (and [null $ ds ptd, not $ null $ ms ptd]) "No 'duplicate' diffs" $
    classify (and [null $ ms ptd, not $ null $ ds ptd]) "No 'missing' diffs" $
    classify (len ptd == 0)  "Trivial" $
    diffs (toList ptd) == allDiffs ptd

main :: IO ()
main = do
    let args = stdArgs {chatty = True, maxSuccess = 1000}
    let tests = [
            quickCheckWithResult args prop_diffs
            ]
    results <- sequence tests
    if all isSuccess results
        then return ()
        else exitFailure
