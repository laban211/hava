module ArgParse where

-- todo: needs more work, not sure If i'll keep this module...
data CommandOptions = CommandOptions
  { command :: !Command,
    description :: !String,
    filePath :: !(Maybe FilePath),
    extraArgs :: ![String]
  }

data Command = Help | Gbc | Bs

-- todo: wip
parseArgs :: [String] -> Either String CommandOptions
-- todo: write something better
parseArgs [] = Left "Missing command, write --help"
parseArgs [command] = Left "Missing command, write --help"
