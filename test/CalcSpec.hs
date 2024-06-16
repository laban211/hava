{-# LANGUAGE RecordWildCards #-}

module CalcSpec
    ( tests
    ) where

import qualified Calc                          as C
import qualified Data.Text                     as T
import qualified Test.HUnit.Base               as HU
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
import           Util                           ( SortableByDate(..)
                                                , alwaysNegative
                                                )
tests :: HU.Test
tests = HU.TestList
    [ HU.TestLabel "Calculate sell profit" testCalcSellProfit
    , HU.TestLabel "Calculate dividend profit" testCalcDividendProfit
    , HU.TestLabel "Calculate total sell and dividend profit for company"
                   testCalcTotalProfitForCompany
    ]

shouldEq :: (Eq a, Show a) => String -> a -> a -> HU.Test
shouldEq msg exp actual = HU.TestCase $ HU.assertEqual msg exp actual

testCalcSellProfit :: HU.Test
testCalcSellProfit = HU.TestList
    [ shouldEq "No profit"             0     (C.calcSellProfit 5 500 3 300)
    , shouldEq "Positive Profit"       30    (C.calcSellProfit 5 500 3 330)
    , shouldEq "Allow negative profit" (-30) (C.calcSellProfit 5 500 3 270)
    ]

testCalcDividendProfit :: HU.Test
testCalcDividendProfit = HU.TestList
    [ shouldEq "No profit"   0  (C.calcTotalDividendProfit [cr 0, cr 0, cr 0])
    , shouldEq "Some profit" 40 (C.calcTotalDividendProfit [cr 20, cr 5, cr 15])
    ]
    where cr = createDividendRowWithPlaceHolders

testCalcTotalProfitForCompany :: HU.Test
testCalcTotalProfitForCompany = HU.TestList
    [ shouldEq
        "No profit"
        0
        (C.calcTotalProfitForCompany $ SortedByDateList [crS 10 1, crB 10 1])
    , shouldEq "No sell, no profit"
               0
               (C.calcTotalProfitForCompany $ SortedByDateList [crB 10 1])
    , shouldEq
        "Positive profit"
        100
        ( C.calcTotalProfitForCompany
        $ SortedByDateList [crS 198 1, crD 2, crB 100 1]
        )
    , shouldEq
        "Positive profit (real case that confused me)"
        1140
        (C.calcTotalProfitForCompany $ SortedByDateList
            [crS 2266 32, crS 2155 32, crB 1973 38, crB 980 20, crB 328 6]
        )
    , shouldEq
        "Negative profit"
        (-100)
        ( C.calcTotalProfitForCompany
        $ SortedByDateList [crS 800 1, crD 100, crB 1000 1]
        )
    , shouldEq
        "With a stock split in the mix"
        1000
        ( C.calcTotalProfitForCompany
        $ SortedByDateList
        -- reverse for readability, first transaction in time first
        $ reverse
              [
                -- Buy 1 stock for 1000
                crB 1000 1
              -- stock split 1 -> 3
              , crSp 2
              -- sell all stocks for 2000 (1000 profit since buy)
              , crS 2000 3
              ]
        )
    ]
  where
    crB amnt qty = TransactionBuySell
        $ createBuySellRowWithPlaceHolders Buy (alwaysNegative amnt) qty
    -- buy amnt is expressed as a negative number ^
    crS amnt qty = TransactionBuySell
        $ createBuySellRowWithPlaceHolders Sell amnt (alwaysNegative qty)
    --     sell qty is expressed as a negative number ^
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
