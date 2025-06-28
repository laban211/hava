module Main where

import CLIHelp
  ( getAdjustedTerminalWidth,
    getUiSizeBasedOnTerminalWidth,
    printHelp,
  )
import Calc
  ( filterBuySell,
    filterProfitYieldingRows,
    groupByCompanySorted,
  )
import qualified Data.ByteString.Lazy as BSL
import Data.Char
  ( chr,
    ord,
  )
import Data.Csv
  ( DecodeOptions (decDelimiter),
    decodeByNameWith,
    defaultDecodeOptions,
  )
import Data.List
  ( find,
    length,
    unlines,
  )
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple
  ( fst,
    snd,
  )
import qualified Data.Vector as V
import Features.BuySell.BuySellTable (createBuySellTable)
import Features.GroupByCompany.GroupByCompanyArgParse (sortableColumnToStr)
import qualified Features.GroupByCompany.GroupByCompanyArgParse as GbcArgParse
import Features.GroupByCompany.GroupByCompanyTable (createGroupByCompanyTable)
import qualified Features.GroupByCompany.GroupByCompanyTypes as GBC
import Parse (parseCsvData)
import qualified PrettyPrint as PP
import System.Environment (getArgs)
import Types.CLITypes (CommandLineOption (..), FlagDoc (..))
import Types.Transaction.GenericTransaction
  ( Transaction,
  )
import Types.Transaction.ParsedTransaction
  ( mapMaybeParsedTransaction,
  )
import Types.UiSize (calcUiSize)
import Types.UtilTypes (SortedByDateList (..))
import Util (SortableByDate (..), unsnoc)

main :: IO ()
main = do
  args <- getArgs
  handleArg args

mainCommands :: [CommandLineOption]
mainCommands =
  [ CommandLineOption
      "buy-sell"
      "bs"
      "Print buy and sell transactions"
      []
      printBuySellHistory,
    CommandLineOption
      "group-by-company"
      "gbc"
      "Print results grouped by company"
      [ FlagDoc
          { flag = "--sort <column> [asc|desc] ",
            flagDescriptionRows =
              [ "Sort table by column (default: asc)",
                -- table order
                -- Företag, Köpt, Sålt, Nuv. balans, Vinst (kr), Utdelning (kr), Vinst sålda (kr)
                "Columns: " ++ createGbcSortOpts [GBC.Company, GBC.Bought, GBC.Sold] ++ ",",
                "         " ++ createGbcSortOpts [GBC.CurrentAmount, GBC.Profit] ++ ",",
                "         " ++ createGbcSortOpts [GBC.Dividend, GBC.ProfitForSold]
              ]
          },
        FlagDoc
          { flag = "--limit <number>",
            flagDescriptionRows = ["Limit number of rows (must be > 1)"]
          }
      ]
      printGroupByComany
  ]
  where
    createGbcSortOpts xs = List.intercalate ", " (map sortableColumnToStr xs)

actionNoOp :: [String] -> FilePath -> IO ()
actionNoOp _ _ = return ()

helpOption :: CommandLineOption
helpOption = CommandLineOption "--help" "-h" "Print help" [] actionNoOp

handleArg :: [String] -> IO ()
handleArg [] = putStrLn "No arguments provided, write --help for instructions"
handleArg [flag] = printHelp mainCommands [helpOption]
-- todo: extraArgs in the middle is a bit complex I guess
handleArg (flag : args) =
  case find (\opt -> flag == longCmd opt || flag == shortCmd opt) mainCommands of
    Just opt -> case unsnoc args of
      Just (extraArgs, filePath) -> action opt extraArgs filePath
      Nothing -> error "Missing filepath"
    Nothing -> error "Unknown flag"

printBuySellHistory :: [String] -> FilePath -> IO ()
printBuySellHistory cliFlags filePath = do
  rows <- readCsv filePath
  termWidth <- getAdjustedTerminalWidth
  let uiSize = calcUiSize termWidth
  let buySell = sortByDate $ filterBuySell rows
  -- todo: not sure if default width 0 is a good idea
  let printContent =
        createBuySellTable
          uiSize
          (fromMaybe 0 termWidth)
          (getSortedByDateList buySell)

  T.putStr printContent

printGroupByComany :: [String] -> FilePath -> IO ()
printGroupByComany cliFlags filePath = do
  rows <- readCsv filePath
  termWidth <- getAdjustedTerminalWidth
  let uiSize = calcUiSize termWidth
  let paredTransactions = mapMaybeParsedTransaction rows
  let groupedByCompany = groupByCompanySorted paredTransactions
  let printContent =
        createGroupByCompanyTable
          uiSize
          (fromMaybe 0 termWidth)
          cliFlags
          (Map.elems groupedByCompany)

  T.putStr printContent

readCsv :: FilePath -> IO [Transaction]
readCsv filePath = do
  csvData <- BSL.readFile filePath
  let rows = parseCsvData csvData
  return rows
