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
    let ptd = PropTestData 13 [Diff {diffType = Duplicate, value = 4, count = 3}] [Diff {diffType = Missing, value = 9, count = 2}]
    let xs = toList ptd
    putStrLn $ show xs
    results <- sequence tests
    if all isSuccess results
        then return ()
        else exitFailure
