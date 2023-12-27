module Calc
    ( filterBuySell
    , groupByCompanySorted
    , calcTotalBuySellProfit
    , calcTotalProfitForCompany
    , calcNumBought
    , calcNumSold
    , calcRemainingAmount
    , calcSellProfit
    , calcTotalDividendProfit
    , extractBuySellRows
    , extractDividendRows
    , filterProfitYieldingRows
    ) where

import           Control.Exception              ( ArithException(DivideByZero)
                                                , Exception
                                                , throw
                                                )
import           Data.Foldable
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Text                     as T
                                                ( pack )
import           Debug.Trace                    ( trace
                                                , traceShowId
                                                )
import           ParseHelper                    ( dive
                                                , mult
                                                )
import           Types.AvaProfitYieldingRow     ( AvaProfitYieldingRow(..)
                                                , createProfitYieldingRow
                                                )
import           Types.AvanzaBuySellRow         ( Action(..)
                                                , AvanzaBuySellRow(..)
                                                , isBuy
                                                , isSell
                                                , transformToBuySellRow
                                                )
import           Types.AvanzaDividendRow        ( Action(..)
                                                , AvanzaDividendRow(..)
                                                )
import           Types.AvanzaRow                ( AvanzaRow(AvanzaRow) )
import qualified Types.AvanzaRow               as AvanzaRow
import           Types.Money                    ( Money(Money, unMoney) )
import           Types.UtilTypes                ( CommonAvanzaRowFields(..)
                                                , HasAction(getAction)
                                                , ProfitYieldingFields(..)
                                                , SortedByDateList(..)
                                                )
import           Util                           ( sortByDate )

filterBuySell :: [AvanzaRow] -> [AvanzaBuySellRow]
filterBuySell = mapMaybe transformToBuySellRow

filterProfitYieldingRows :: [AvanzaRow] -> [AvaProfitYieldingRow]
filterProfitYieldingRows = mapMaybe createProfitYieldingRow

extractBuySellRows
    :: Foldable f => f AvaProfitYieldingRow -> [AvanzaBuySellRow]
extractBuySellRows = foldMap extractBuySellRow
  where
    extractBuySellRow :: AvaProfitYieldingRow -> [AvanzaBuySellRow]
    extractBuySellRow (BuySellRowWrapper buySellRow) = [buySellRow]
    extractBuySellRow _                              = []

extractDividendRows
    :: Foldable f => f AvaProfitYieldingRow -> [AvanzaDividendRow]
extractDividendRows = foldMap extractDividendRow
  where
    extractDividendRow :: AvaProfitYieldingRow -> [AvanzaDividendRow]
    extractDividendRow (DividendRowWrapper dividendRow) = [dividendRow]
    extractDividendRow _ = []

groupByCompanySorted
    :: (CommonAvanzaRowFields a, Foldable f, Show a)
    => f a
    -> Map Text (SortedByDateList a)
groupByCompanySorted input =
    let toMapEntry x = Map.singleton (getCompany x) [x]
        mergedMap = foldl' (Map.unionWith (<>))
                           Map.empty
                           (map toMapEntry (toList input))
    in  Map.map sortByDate mergedMap

-- todo maybe change name at some point
calcTotalBuySellProfit :: [AvanzaBuySellRow] -> Money
calcTotalBuySellProfit = sum . map getAmount

data StateCSSPBC = StateCSSPBC
    { currQuantity        :: Int
    , totalAmount         :: Money
    , totalDividendProfit :: Money
    , totalSellProfit     :: Money
    }
    deriving Show

calcTotalProfitForCompany :: SortedByDateList AvaProfitYieldingRow -> Money
calcTotalProfitForCompany rows =
    let result = foldr processRow initState rows
    in  totalSellProfit result + totalDividendProfit result
  where
    initState = StateCSSPBC { currQuantity        = 0
                            , totalAmount         = Money 0
                            , totalDividendProfit = Money 0
                            , totalSellProfit     = Money 0
                            }

    processRow :: AvaProfitYieldingRow -> StateCSSPBC -> StateCSSPBC
    -- buy/sell
    processRow (BuySellRowWrapper row) state = case getAction row of
        Buy ->
            let newState = state
                    { currQuantity = currQuantity state + getQuantity row
                    , totalAmount  = totalAmount state + abs (getAmount row)
                   --        buy amount will be negative ^
                    }
            in  newState
        Sell ->
            let
                prevQuanitity = currQuantity state
                newQuantity   = prevQuanitity - abs (getQuantity row)
            --              sell qty will be negative ^
                newTotalAmount =
                    totalAmount state + newTotalSellProfit - getAmount row
                newTotalSellProfit = totalSellProfit state + calcSellProfit
                    prevQuanitity
                    (totalAmount state)
                    (abs $ getQuantity row)
                    (getAmount row)

                newState = state { currQuantity    = newQuantity
                                 , totalAmount     = newTotalAmount
                                 , totalSellProfit = newTotalSellProfit
                                 }
            in
                newState
    -- dividend
    processRow (DividendRowWrapper row) state = state
        { totalDividendProfit = totalDividendProfit state + abs (getAmount row)
        }



newtype CustomException = DivideByZeroWithArg String
    deriving Show
instance Exception CustomException

calcSellProfit :: Int -> Money -> Int -> Money -> Money
calcSellProfit quantity amount soldQuantity soldAmount
    | soldQuantity == 0 = throw (DivideByZeroWithArg "soldQuantity")
    | otherwise         = (avgSell - avgBuy) * fromIntegral soldQuantity
  where
    avgBuy = if amount == 0 && quantity == 0
        -- there are sutiations where this makes sense. E.g. If a company has
        -- changed name
        then 0
        else amount / fromIntegral quantity
    avgSell = soldAmount / fromIntegral soldQuantity


calcNumBought :: [AvanzaBuySellRow] -> Int
calcNumBought = sum . map getQuantity . filter isBuy

calcNumSold :: [AvanzaBuySellRow] -> Int
calcNumSold = abs . sum . map getQuantity . filter isSell

calcRemainingAmount :: [AvanzaBuySellRow] -> Int
calcRemainingAmount = sum . map getQuantity

calcTotalDividendProfit :: [AvanzaDividendRow] -> Money
calcTotalDividendProfit = sum . map getAmount
