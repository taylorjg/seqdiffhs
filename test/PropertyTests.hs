import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit (exitFailure)
import SeqDiff
import PropTestData
import ArbitraryPropTestData

prop_PropTestData :: PropTestData -> Bool
prop_PropTestData ptd =
    actual == expected
    where
        expected = allDiffs ptd
        actual = diffs xs
        xs = toList ptd

main :: IO ()
main = do
    let args = stdArgs {chatty = True, maxSuccess = 5}
    let tests = [verboseCheckWithResult args prop_PropTestData]
    results <- sequence tests
    if all isSuccess results
        then return ()
        else exitFailure
