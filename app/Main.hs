module Main where

import           CLIHelp                        ( getAdjustedTerminalWidth
                                                , getUiSizeBasedOnTerminalWidth
                                                , printHelp
                                                )
import           Calc                           ( filterBuySell
                                                , filterProfitYieldingRows
                                                , groupByCompanySorted
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Csv                       ( DecodeOptions(decDelimiter)
                                                , decodeByNameWith
                                                , defaultDecodeOptions
                                                )
import qualified Data.List                     as List
import           Data.List                      ( find
                                                , length
                                                , unlines
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import qualified Data.Vector                   as V
import           Parse                    ( parseCsvData )
import qualified PrettyPrint                   as PP
import           System.Environment             ( getArgs )
import           Tables.BuySellTable            ( createBuySellTable )
import           Tables.GroupByCompanyTable     ( createGroupByCompanyTable )
import           Types.CLITypes                 ( CommandLineOption(..) )
import           Types.Transaction.GenericTransaction
                                                ( Transaction )
import           Types.Transaction.ParsedTransaction
                                                ( mapMaybeParsedTransaction )
import           Types.UiSize                   ( calcUiSize )
import           Types.UtilTypes                ( SortedByDateList(..) )
import           Util                           ( SortableByDate(..) )


main :: IO ()
main = do
  args <- getArgs
  handleArg args

mainOptions :: [CommandLineOption]
mainOptions =
  [ CommandLineOption "--buy-sell"
                      "-bs"
                      "Print buy and sell transactions"
                      printBuySellHistory
  , CommandLineOption "--group-by-company"
                      "-gbc"
                      "Print results grouped by company"
                      printGroupByComany
  ]

actionNoOp :: FilePath -> IO ()
actionNoOp _ = return ()

helpOption :: CommandLineOption
helpOption = CommandLineOption "--help" "-h" "Print help" actionNoOp

handleArg :: [String] -> IO ()
handleArg [] = putStrLn "No arguments provided, write --help for instructions"
handleArg [flag] = printHelp (helpOption : mainOptions)
handleArg (flag : filePath : _) =
  case find (\opt -> flag == longArg opt || flag == shortArg opt) mainOptions of
    Just opt -> action opt filePath
    Nothing  -> error "Unknown flag"


printBuySellHistory :: FilePath -> IO ()
printBuySellHistory filePath = do
  rows      <- readCsv filePath
  termWidth <- getAdjustedTerminalWidth
  let uiSize  = calcUiSize termWidth
  let buySell = sortByDate $ filterBuySell rows
  -- todo: not sure if default width 0 is a good idea
  let printContent = createBuySellTable uiSize
                                        (fromMaybe 0 termWidth)
                                        (getSortedByDateList buySell)

  T.putStr printContent

printGroupByComany :: FilePath -> IO ()
printGroupByComany filePath = do
  rows      <- readCsv filePath
  termWidth <- getAdjustedTerminalWidth
  let uiSize            = calcUiSize termWidth
  let paredTransactions = mapMaybeParsedTransaction rows
  let groupedByCompany  = groupByCompanySorted paredTransactions
  let printContent = createGroupByCompanyTable uiSize
                                               (fromMaybe 0 termWidth)
                                               (Map.elems groupedByCompany)

  T.putStr printContent

readCsv :: FilePath -> IO [Transaction]
readCsv filePath = do
  csvData <- BSL.readFile filePath
  let rows = parseCsvData csvData
  return rows
