import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit (exitFailure)
import SeqDiff
import PropTestData
import ArbitraryPropTestData

prop_PropTestData :: PropTestData -> Bool
prop_PropTestData _ = True

main :: IO ()
main = do
    let args = stdArgs {chatty = True, maxSuccess = 500}
    let tests = [verboseCheckWithResult args prop_PropTestData]
    results <- sequence tests
    if all isSuccess results
        then return ()
        else exitFailure
