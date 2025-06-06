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
import Types.CLITypes (CommandLineOption (..))
import Types.PrintableCell
  ( CellWidth (..),
    FixedWidth (..),
    createDefaultCell,
  )
import Types.UiSize
  ( UiSize (..),
    calcUiSize,
  )

printHelp :: [CommandLineOption] -> [CommandLineOption] -> IO ()
printHelp commands options = do
  printUsage
  T.putStrLn (T.pack "")
  printCmdLineCommands commands
  T.putStrLn (T.pack "")
  printCmdLineOpts options

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

printCmdLineCommands :: [CommandLineOption] -> IO ()
printCmdLineCommands options = do
  let createCellWithOpts opt =
        [ createDefaultCell
            (longCmd opt ++ ", " ++ shortCmd opt)
            FixedWidth {s = 27},
          createDefaultCell (description opt) FixedWidth {s = 40}
        ]
  let printRow opts = PP.createColumnsRow Small (createCellWithOpts opts) 2

  printSection "Commands"
  mapM_ (T.putStrLn . printRow) options

printCmdLineOpts :: [CommandLineOption] -> IO ()
printCmdLineOpts options = do
  let createCellWithOpts opt =
        [ createDefaultCell
            (longCmd opt ++ ", " ++ shortCmd opt)
            FixedWidth {s = 27},
          createDefaultCell (description opt) FixedWidth {s = 40}
        ]
  let printRow opts = PP.createColumnsRow Small (createCellWithOpts opts) 2

  printSection "Options"
  mapM_ (T.putStrLn . printRow) options

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
