import Test.QuickCheck
import Test.QuickCheck.Test
import SeqDiff
import System.Exit (exitFailure)

prop_True1 :: Int -> Bool
prop_True1 _ = True

prop_True2 :: Int -> Bool
prop_True2 n = if n > 10 then False else True

main :: IO ()
main = do
    let args = stdArgs {chatty = True, maxSuccess = 500}
    r1 <- quickCheckWithResult args prop_True1
    r2 <- quickCheckWithResult args prop_True2
    if all isSuccess [r1, r2]
        then return ()
        else exitFailure
