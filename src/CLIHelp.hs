module CLIHelp
    ( printHelp
    ) where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified PrettyPrint                   as PP
import           Types.CLITypes                 ( CommandLineOption(..) )



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
    let
        printRow = PP.createNoSepTableRow
            [ PP.PrintableCell (T.pack "hava [option] [path-to-csv-file]")
                               (aMaxLen + 3)
            , PP.PrintableCell
                (T.pack "Run Hava in <option>-mode for Avanza transaction file")
                bMaxLen
            ]

    T.putStrLn (T.pack "Usage:")
    T.putStrLn printRow


printCmdLineOpts :: [CommandLineOption] -> IO ()
printCmdLineOpts options = do
    let printRow opt = PP.createNoSepTableRow
            [ PP.PrintableCell (T.pack $ longArg opt ++ ", " ++ shortArg opt) 27
            , PP.PrintableCell (T.pack $ description opt) 40
            ]

    T.putStrLn (T.pack "Options:")
    mapM_ (T.putStrLn . printRow) options
