module Types.CLITypes
    ( CommandLineOption(..)
    ) where

data CommandLineOption = CommandLineOption
    { longArg     :: String
    , shortArg    :: String
    , description :: String
    , action      :: FilePath -> IO ()
    }
