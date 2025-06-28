{-# OPTIONS_GHC -Wno-missing-fields #-}

module CLIHelp
  ( printHelp,
    getAdjustedTerminalWidth,
    getUiSizeBasedOnTerminalWidth,
  )
where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified PrettyPrint as PP
import System.Console.Terminal.Size
  ( Window (..),
    size,
  )
import Types.CLITypes (CommandLineOption (..), FlagDoc (..))
import Types.PrintableCell
  ( CellWidth (..),
    FixedWidth (..),
    PrintableCell,
    createDefaultCell,
  )
import Types.UiSize
  ( UiSize (..),
    calcUiSize,
  )

-- todo: separata commands and options..?
printHelp :: [CommandLineOption] -> [CommandLineOption] -> IO ()
printHelp commands options = do
  printUsage
  T.putStrLn (T.pack "")
  printCmdLineOpts "Commands" commands
  T.putStrLn (T.pack "")
  printCmdLineOpts "Global options" options

usages :: [(String, String)]
usages =
  [ ( "hava [command] [path-to-csv-file]",
      "Run Hava in <command>-mode for Avanza transactions file"
    )
  ]

printUsage :: IO ()
printUsage = do
  let aMaxLen = maximum $ map (length . fst) usages
  let bMaxLen = maximum $ map (length . snd) usages
  let cells =
        [ createDefaultCell
            "hava [command] [path-to-csv-file]"
            FixedWidth {s = aMaxLen + 3},
          createDefaultCell
            "Run Hava in <command>-mode for Avanza transaction file"
            FixedWidth {s = bMaxLen}
        ]
  let printRow = PP.createColumnsRow Small cells 2

  printSection "Usage"
  T.putStrLn printRow

-- todo: better name
indent1, indent2, indent3 :: Int
indent1 = 2
indent2 = 4
indent3 = 6

createSimpleRowWithIndent :: Int -> String -> T.Text
createSimpleRowWithIndent indent content = PP.createColumnsRow Small [createDefaultCell content FixedWidth {s = 35}] indent

createSimpleRowInd1 :: String -> T.Text
createSimpleRowInd1 = createSimpleRowWithIndent indent1

createSimpleRowInd2 :: String -> T.Text
createSimpleRowInd2 = createSimpleRowWithIndent indent2

{- createMultiCellRowWithIndent :: Int -> [String] -> T.Text
createMultiCellRowWithIndent indent cellContents = PP.createColumnsRow Small (map (\c -> createDefaultCell c FixedWidth {s = 40}) cellContents) indent

createMultiCellRowInd3 :: [String] -> T.Text
createMultiCellRowInd3 = createMultiCellRowWithIndent indent3 -}

create2CellRowWithIndent :: Int -> String -> String -> T.Text
create2CellRowWithIndent indent cell1 cell2 =
  PP.createColumnsRow
    Small
    -- aim for 80 total width
    [ createDefaultCell cell1 FixedWidth {s = 35 - indent},
      createDefaultCell cell2 FixedWidth {s = 45}
    ]
    indent

create2CellRowInd3 :: String -> String -> T.Text
create2CellRowInd3 = create2CellRowWithIndent indent3

printCmdLineOpts :: String -> [CommandLineOption] -> IO ()
printCmdLineOpts sectionName options = do
  let allRows = List.intercalate [T.empty] (map renderOption options)

  printSection sectionName
  mapM_ T.putStrLn allRows
  where
    -- todo: move out
    leftPad :: Int -> Char -> String -> String
    leftPad n c s = replicate (n - length s) c ++ s

    renderOption :: CommandLineOption -> [T.Text]
    renderOption opt =
      let cmdName = createSimpleRowInd1 (longCmd opt ++ ", " ++ shortCmd opt)
          cmdDesc = createSimpleRowInd2 (description opt)
          flagDescSection = createSimpleRowInd2 "options:"
          flagDescRows = concatMap createFlagDocRows (commandFlags opt)
          maybeFlagSection =
            if null flagDescRows
              then []
              else flagDescSection : flagDescRows
       in cmdName : cmdDesc : maybeFlagSection

    -- flag description
    createFlagDocRows :: FlagDoc -> [T.Text]
    createFlagDocRows x =
      let descRows = flagDescriptionRows x
          fstRow = create2CellRowInd3 (flag x) (head descRows)
          restRows = map (create2CellRowInd3 "") (tail descRows)
          paddingRow = createSimpleRowInd2 ""
       in fstRow : restRows ++ [paddingRow]

printSection :: String -> IO ()
printSection x = T.putStrLn (T.pack $ x <> ":")

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = do
  fmap width <$> size

getAdjustedTerminalWidth :: IO (Maybe Int)
getAdjustedTerminalWidth = fmap (fmap (\w -> w - 10)) getTerminalWidth

getUiSizeBasedOnTerminalWidth :: IO UiSize
getUiSizeBasedOnTerminalWidth = do
  calcUiSize <$> getAdjustedTerminalWidth
