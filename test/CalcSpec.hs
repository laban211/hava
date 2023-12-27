{-# LANGUAGE RecordWildCards #-}

module CalcSpec
    ( tests
    ) where

import qualified Calc                          as C
import qualified Data.Text                     as T
import qualified Test.HUnit.Base               as HU
import           Types.AvaProfitYieldingRow     ( AvaProfitYieldingRow(..) )
import           Types.AvanzaBuySellRow         ( Action(..)
                                                , AvanzaBuySellRow(..)
                                                )
import qualified Types.AvanzaBuySellRow        as AvanzaBuySellRow
import qualified Types.AvanzaDividendRow       as AvanzaDividendRow
import           Types.AvanzaDividendRow        ( Action(Dividend)
                                                , AvanzaDividendRow(..)
                                                )
import           Types.Money                    ( Money )
import           Types.UtilTypes                ( SortedByDateList
                                                    ( SortedByDateList
                                                    )
                                                )
import           Util                           ( alwaysNegative )

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
    ]
  where
    crB amnt qty = BuySellRowWrapper
        $ createBuySellRowWithPlaceHolders Buy (alwaysNegative amnt) qty
    -- buy amnt is expressed as a negative number ^
    crS amnt qty = BuySellRowWrapper
        $ createBuySellRowWithPlaceHolders Sell amnt (alwaysNegative qty)
    --     sell qty is expressed as a negative number ^
    crD amnt = DividendRowWrapper $ createDividendRowWithPlaceHolders amnt

-- Utility functions for test suite

createBuySellRowWithPlaceHolders
    :: AvanzaBuySellRow.Action -> Money -> Int -> AvanzaBuySellRow
createBuySellRowWithPlaceHolders action amnt qty = AvanzaBuySellRow
    { date     = placeHolder
    , account  = placeHolder
    , action   = action
    , company  = placeHolder
    -- We care about this field
    , quantity = qty
    , rate     = 0
    -- We care about this field
    , amount   = amnt
    , courtage = 0
    , currency = placeHolder
    , isin     = placeHolder
    }
    where placeHolder = T.pack "placeHolder"

createDividendRowWithPlaceHolders :: Money -> AvanzaDividendRow
createDividendRowWithPlaceHolders amnt = AvanzaDividendRow
    { date     = placeHolder
    , account  = placeHolder
    , action   = Dividend
    , company  = placeHolder
    , quantity = 0
    , rate     = 4
    -- this is the only field that matters
    , amount   = amnt
    , courtage = 0
    , currency = placeHolder
    , isin     = placeHolder
    }
    where placeHolder = T.pack "placeHolder"
