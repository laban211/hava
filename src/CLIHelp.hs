{-# OPTIONS_GHC -Wno-missing-fields #-}
module CLIHelp
    ( printHelp
    , getAdjustedTerminalWidth
    , getUiSizeBasedOnTerminalWidth
    ) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified PrettyPrint                   as PP
import           System.Console.Terminal.Size   ( Window(..)
                                                , size
                                                )
import           Types.CLITypes                 ( CommandLineOption(..) )
import           Types.PrintableCell            ( CellWidth(..)
                                                , FixedWidth(..)
                                                , createDefaultCell
                                                )
import           Types.UiSize                   ( UiSize(..)
                                                , calcUiSize
                                                )



printHelp :: [CommandLineOption] -> IO ()
printHelp options = do
    printUsage
    T.putStrLn (T.pack "")
    printCmdLineOpts options

usages :: [(String, String)]
usages =
    [ ( "hava [option] [path-to-csv-file]"
      , "Run Hava in <option>-mode for Avanza transactions file"
      )
    ]

printUsage :: IO ()
printUsage = do
    let aMaxLen = maximum $ map (length . fst) usages
    let bMaxLen = maximum $ map (length . snd) usages
    let cells =
            [ createDefaultCell "hava [option] [path-to-csv-file]"
                                FixedWidth { s = aMaxLen + 3 }
            , createDefaultCell
                "Run Hava in <option>-mode for Avanza transaction file"
                FixedWidth { s = bMaxLen }
            ]
    let printRow = PP.createColumnsRow Small cells 2

    printSection "Usage"
    T.putStrLn printRow


printCmdLineOpts :: [CommandLineOption] -> IO ()
printCmdLineOpts options = do
    let createCellWithOpts opt =
            [ createDefaultCell (longArg opt ++ ", " ++ shortArg opt)
                                FixedWidth { s = 27 }
            , createDefaultCell (description opt) FixedWidth { s = 40 }
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

