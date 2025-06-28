module Types.CLITypes
  ( CommandLineOption (..),
    FlagDoc (..),
  )
where

data CommandLineOption = CommandLineOption
  { longCmd :: String,
    shortCmd :: String,
    description :: String,
    commandFlags :: [FlagDoc],
    action :: [String] -> FilePath -> IO ()
  }

data FlagDoc = FlagDoc
  { flag :: String,
    flagDescription :: String,
    defaultValue :: String
  }
