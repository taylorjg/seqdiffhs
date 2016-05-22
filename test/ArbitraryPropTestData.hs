module ArbitraryPropTestData where

import Test.QuickCheck
import PropTestData

genPropTestData :: Gen PropTestData
genPropTestData = return $ PropTestData 10 [] []

shrinkPropTestData :: PropTestData -> [PropTestData]
shrinkPropTestData ptd = []

instance Arbitrary PropTestData where
    arbitrary = genPropTestData
    shrink = shrinkPropTestData
