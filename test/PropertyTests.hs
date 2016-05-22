import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit (exitFailure)
import SeqDiff
import PropTestData
import ArbitraryPropTestData

prop_Diffs :: PropTestData -> Bool
prop_Diffs ptd = diffs (toList ptd) == allDiffs ptd

main :: IO ()
main = do
    let args = stdArgs {chatty = True, maxSuccess = 100}
    let tests = [verboseCheckWithResult args prop_Diffs]
    results <- sequence tests
    if all isSuccess results
        then return ()
        else exitFailure
