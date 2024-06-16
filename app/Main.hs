module Main where

import           CLIHelp                        ( printHelp )
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
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import qualified Data.Vector                   as V
import           ParseHelper                    ( processData )
import qualified PrettyPrint                   as PP
import           System.Environment
import           Types.CLITypes                 ( CommandLineOption(..) )
import           Types.Transaction.GenericTransaction
                                                ( Transaction )
import           Types.Transaction.ParsedTransaction
                                                ( mapMaybeParsedTransaction )
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
  rows <- readCsv filePath
  let buySell      = sortByDate $ filterBuySell rows
  let printContent = PP.createBuySellTable $ getSortedByDateList buySell

  T.putStr printContent

printGroupByComany :: FilePath -> IO ()
printGroupByComany filePath = do
  rows <- readCsv filePath
  let paredTransactions = mapMaybeParsedTransaction rows
  let groupedByCompany  = groupByCompanySorted paredTransactions
  let printContent = PP.createGroupByCompanyTable (Map.elems groupedByCompany)

  T.putStr printContent

readCsv :: FilePath -> IO [Transaction]
readCsv filePath = do
  csvData <- BSL.readFile filePath
  let rows = processData csvData
  return rows
