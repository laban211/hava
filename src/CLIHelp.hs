module CLIHelp
    ( printHelp
    ) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified PrettyPrint                   as PP
import           Types.CLITypes                 ( CommandLineOption(..) )
import           Types.PrintableCell            ( PrintableCell(..)
                                                , createDefaultCell
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
            [ createDefaultCell "hava [option] [path-to-csv-file]" (aMaxLen + 3)
            , createDefaultCell
                "Run Hava in <option>-mode for Avanza transaction file"
                bMaxLen
            ]
    let printRow = PP.createColumnsRow cells 2

    printSection "Usage"
    T.putStrLn printRow


printCmdLineOpts :: [CommandLineOption] -> IO ()
printCmdLineOpts options = do
    let createCellWithOpts opt =
            [ createDefaultCell (longArg opt ++ ", " ++ shortArg opt) 27
            , createDefaultCell (description opt)                     40
            ]
    let printRow opts = PP.createColumnsRow (createCellWithOpts opts) 2

    printSection "Options"
    mapM_ (T.putStrLn . printRow) options

printSection :: String -> IO ()
printSection x = T.putStrLn (T.pack $ x <> ":")
