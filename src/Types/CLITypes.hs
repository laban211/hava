module Types.CLITypes
  ( CommandLineOption (..),
  )
where

data CommandLineOption = CommandLineOption
  { longCmd :: String,
    shortCmd :: String,
    description :: String,
    action :: [String] -> FilePath -> IO ()
  }
