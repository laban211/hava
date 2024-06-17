module CalcSpec
    ( spec
    ) where

import qualified Calc                          as C
import qualified Data.Text                     as T
import           Test.Hspec
import           Types.Money                    ( Money )
import           Types.Transaction.GenericTransaction
                                                ( GenericTransaction(..) )
import           Types.Transaction.ParsedTransaction
                                                ( ParsedTransaction(..) )
import           Types.Transaction.TransactionBuySell
                                                ( Action(..)
                                                , TransactionBuySell(..)
                                                )
import qualified Types.Transaction.TransactionBuySell
                                               as TransactionBuySell
import           Types.Transaction.TransactionDividend
                                                ( Action(..)
                                                , TransactionDividend(..)
                                                )
import           Types.Transaction.TransactionProfitYielding
                                                ( TransactionProfitYielding(..)
                                                )
import           Types.Transaction.TransactionSplit
                                                ( Action(..)
                                                , TransactionSplit(..)
                                                )
import           Types.UtilTypes                ( SortedByDateList(..) )
import           Util                           ( alwaysNegative )

spec :: Spec
spec = do
    describe "Calculate sell profit"     testCalcSellProfit
    describe "Calculate dividend profit" testCalcDividendProfit
    describe "Calculate total sell and dividend profit for company"
             testCalcTotalProfitForCompany

testCalcSellProfit :: Spec
testCalcSellProfit = do
    it "should result in no profit" $ do
        C.calcSellProfit 5 500 3 300 `shouldBe` 0
    it "should result in positive profit" $ do
        C.calcSellProfit 5 500 3 330 `shouldBe` 30
    it "should allow negative profit" $ do
        C.calcSellProfit 5 500 3 270 `shouldBe` (-30)

testCalcDividendProfit :: Spec
testCalcDividendProfit = do
    it "should result in no profit" $ do
        C.calcTotalDividendProfit [cr 0, cr 0, cr 0] `shouldBe` 0
    it "should result in some profit" $ do
        C.calcTotalDividendProfit [cr 20, cr 5, cr 15] `shouldBe` 40
    where cr = createDividendRowWithPlaceHolders

testCalcTotalProfitForCompany :: Spec
testCalcTotalProfitForCompany = do
    it "should result in no profit" $ do
        C.calcTotalProfitForCompany (SortedByDateList [crS 10 1, crB 10 1])
            `shouldBe` 0
    it "should handle 'no sell, no profit'" $ do
        C.calcTotalProfitForCompany (SortedByDateList [crB 10 1]) `shouldBe` 0
    it "should result in positive profit" $ do
        C.calcTotalProfitForCompany
                (SortedByDateList [crS 198 1, crD 2, crB 100 1])
            `shouldBe` 100
    it "can handle positive profit (real case that confused me)" $ do
        C.calcTotalProfitForCompany
                (SortedByDateList
                    [ crS 2266 32
                    , crS 2155 32
                    , crB 1973 38
                    , crB 980  20
                    , crB 328  6
                    ]
                )
            `shouldBe` 1140
    it "should result in negative profit" $ do
        C.calcTotalProfitForCompany
                (SortedByDateList [crS 800 1, crD 100, crB 1000 1])
            `shouldBe` (-100)
    it "can handle the 'stock split' transaction"
        $          do
                       -- reverse for readability, first transaction in time first
                       C.calcTotalProfitForCompany
                           (SortedByDateList $ reverse
                               [
                                 -- Buy 1 stock for 1000
                                 crB 1000 1
                                 -- stock split 1 -> 3
                               , crSp 2
                                 -- sell all stocks for 2000 (1000 profit since buy)
                               , crS 2000 3
                               ]
                           )
        `shouldBe` 1000
  where
    crB amnt qty = TransactionBuySell
        $ createBuySellRowWithPlaceHolders Buy (alwaysNegative amnt) qty
    crS amnt qty = TransactionBuySell
        $ createBuySellRowWithPlaceHolders Sell amnt (alwaysNegative qty)
    crD amnt = TransactionDividend $ createDividendRowWithPlaceHolders amnt
    crSp qty = TransactionSplit $ createSplitRowWithPlaceHolders qty

-- Utility functions for test suite

createBuySellRowWithPlaceHolders
    :: TransactionBuySell.Action -> Money -> Int -> TransactionBuySell
createBuySellRowWithPlaceHolders action amnt qty = GenericTransaction
    { date     = placeHolder
    , account  = placeHolder
    , action   = action
    , company  = placeHolder
    , quantity = qty -- We care about this field
    , rate     = 0
    , amount   = amnt -- We care about this field
    , courtage = 0
    , currency = placeHolder
    , isin     = placeHolder
    }
    where placeHolder = T.pack "placeHolder"

createDividendRowWithPlaceHolders :: Money -> TransactionDividend
createDividendRowWithPlaceHolders amnt = GenericTransaction
    { date     = placeHolder
    , account  = placeHolder
    , action   = Dividend
    , company  = placeHolder
    , quantity = 0
    , rate     = 4
    , amount   = amnt -- this is the only field that matters
    , courtage = 0
    , currency = placeHolder
    , isin     = placeHolder
    }
    where placeHolder = T.pack "placeHolder"

createSplitRowWithPlaceHolders :: Int -> TransactionSplit
createSplitRowWithPlaceHolders qty = GenericTransaction
    { date     = placeHolder
    , account  = placeHolder
    , action   = Split
    , company  = placeHolder
    , quantity = qty -- this is the only field that matters
    , rate     = ()
    , amount   = ()
    , courtage = ()
    , currency = placeHolder
    , isin     = placeHolder
    }
    where placeHolder = T.pack "placeHolder"
