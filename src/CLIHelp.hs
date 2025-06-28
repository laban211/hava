{-# OPTIONS_GHC -Wno-missing-fields #-}

module CLIHelp
  ( printHelp,
    getAdjustedTerminalWidth,
    getUiSizeBasedOnTerminalWidth,
  )
where

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
  printCmdLineOpts "Options" options

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

printCmdLineOpts :: String -> [CommandLineOption] -> IO ()
printCmdLineOpts sectionName options = do
  let allRows = concatMap renderOption options

  printSection sectionName
  mapM_ T.putStrLn allRows
  where
    -- todo: move out
    leftPad :: Int -> Char -> String -> String
    leftPad n c s = replicate (n - length s) c ++ s

    renderOption :: CommandLineOption -> [T.Text]
    renderOption opt =
      let mainRow = PP.createColumnsRow Small (createCmdDescCells opt) 2
          -- todo: not sure about leftPad bellow..
          descFlagSection = PP.createColumnsRow Small [createDefaultCell (leftPad 2 ' ' "options") FixedWidth {s = 27}] 2
          flagRows = map (\f -> PP.createColumnsRow Small (createFlagDocRow f) 4) (commandFlags opt)
       in mainRow : flagRows

    -- command and description

    createCmdDescCells opt =
      [ createDefaultCell
          (longCmd opt ++ ", " ++ shortCmd opt)
          FixedWidth {s = 27},
        createDefaultCell (description opt) FixedWidth {s = 40}
      ]

    createCmdDescRows opts = PP.createColumnsRow Small (createCmdDescCells opts) 2

    -- flag description
    createFlagDocRow :: FlagDoc -> [PrintableCell]
    createFlagDocRow x =
      [ createDefaultCell (leftPad 2 ' ' (flag x)) FixedWidth {s = 27},
        createDefaultCell (leftPad 2 ' ' (flagDescription x)) FixedWidth {s = 27}
      ]

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
