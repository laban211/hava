module Calc
    ( filterBuySell
    , filterProfitYieldingRows
    , groupByCompanySorted
    , calcTotalBuySellProfit
    , calcTotalProfitForCompany
    , calcNumBought
    , calcNumSold
    , calcRemainingAmount
    , calcSellProfit
    , calcTotalDividendProfit
    ) where

import           Control.Exception              ( ArithException(DivideByZero)
                                                , Exception
                                                , throw
                                                )
import           Data.Foldable                  ( Foldable(foldl', toList) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Debug.Trace                    ( trace
                                                , traceShowId
                                                )
import           Types.Money                    ( Money(..) )
import           Types.Transaction.GenericTransaction
                                                ( Transaction
                                                , getAction
                                                , getAmount
                                                , getCompany
                                                , getQuantity
                                                )
import           Types.Transaction.ParsedTransaction
                                                ( ParsedTransaction(..)
                                                , extractGenericTransaction
                                                )
import           Types.Transaction.TransactionBuySell
                                                ( Action(..)
                                                , TransactionBuySell(..)
                                                , isBuy
                                                , isSell
                                                , transformToBuySell
                                                )
import           Types.Transaction.TransactionDividend
                                                ( Action(..)
                                                , TransactionDividend(..)
                                                , transformToDividend
                                                )
import           Types.Transaction.TransactionProfitYielding
                                                ( TransactionProfitYielding(..)
                                                , extractBuySellRows
                                                , extractDividendRows
                                                )
import           Types.UtilTypes                ( SortedByDateList(..) )
import           Util                           ( SortableByDate(..) )

filterBuySell :: [Transaction] -> [TransactionBuySell]
filterBuySell = mapMaybe transformToBuySell

filterProfitYieldingRows :: [Transaction] -> [TransactionProfitYielding]
filterProfitYieldingRows = concatMap transform
  where
    transform :: Transaction -> [TransactionProfitYielding]
    transform x =
        maybeToList (TransactionBuySellWrapper <$> transformToBuySell x)
            ++ maybeToList
                   (TransactionDividendWrapper <$> transformToDividend x)

groupByCompanySorted
    :: Foldable f
    => f ParsedTransaction
    -> Map Text (SortedByDateList ParsedTransaction)
groupByCompanySorted input =
    let
        toMapEntry x =
            Map.singleton (getCompany $ extractGenericTransaction x) [x]
        mergedMap = foldl' (Map.unionWith (<>))
                           Map.empty
                           (map toMapEntry (toList input))
    in
        Map.map sortByDate mergedMap

-- todo maybe change name at some point
calcTotalBuySellProfit :: [TransactionBuySell] -> Money
calcTotalBuySellProfit = sum . map getAmount

data StateCSSPBC = StateCSSPBC
    { currQuantity        :: Int
    , totalAmount         :: Money
    , totalDividendProfit :: Money
    , totalSellProfit     :: Money
    }
    deriving Show

calcTotalProfitForCompany :: SortedByDateList ParsedTransaction -> Money
calcTotalProfitForCompany rows =
    let result = foldr processRow initState rows
    in  totalSellProfit result + totalDividendProfit result
  where
    initState = StateCSSPBC { currQuantity        = 0
                            , totalAmount         = Money 0
                            , totalDividendProfit = Money 0
                            , totalSellProfit     = Money 0
                            }

    processRow :: ParsedTransaction -> StateCSSPBC -> StateCSSPBC
    -- buy/sell
    processRow (TransactionBuySell row) state = case getAction row of
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
    processRow (TransactionDividend row) state = state
        { totalDividendProfit = totalDividendProfit state + abs (getAmount row)
        }
    -- split 
    processRow (TransactionSplit row) state =
        state { currQuantity = currQuantity state + getQuantity row }

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


calcNumBought :: [TransactionBuySell] -> Int
calcNumBought = sum . map getQuantity . filter isBuy

calcNumSold :: [TransactionBuySell] -> Int
calcNumSold = abs . sum . map getQuantity . filter isSell

calcRemainingAmount :: [ParsedTransaction] -> Int
calcRemainingAmount = sum . map getQuantity'
  where
    getQuantity' :: ParsedTransaction -> Int
    getQuantity' (TransactionBuySell x) = getQuantity x
    getQuantity' (TransactionSplit   x) = getQuantity x
    getQuantity' _                      = 0

calcTotalDividendProfit :: [TransactionDividend] -> Money
calcTotalDividendProfit = sum . map getAmount
