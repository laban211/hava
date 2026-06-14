{-# LANGUAGE FlexibleInstances #-}

-- | A single row of Avanza's per-account holdings ("positioner") export — i.e.
--   one current position held in one account. The same instrument can therefore
--   appear in several rows (once per account it's held in), distinguished by
--   'account' (Kontonummer).
module Types.Position
  ( Position (..),
    expectedPositionHeaders,
  )
where

import qualified Data.ByteString.UTF8 as BSU
import Data.Csv
  ( FromNamedRecord (..),
    (.:),
  )
import Data.Text (Text)
import Types.Money (Money)
import Types.UtilTypes
  ( parseSvDouble,
    parseSvMoney,
  )

data Position = Position
  { account :: !Text, -- Kontonummer
    name :: !Text, -- Namn
    shortName :: !Text, -- Kortnamn
    volume :: !Double, -- Volym (fractional for funds)
    marketValue :: !Money, -- Marknadsvärde
    gavSek :: !Money, -- GAV (SEK)
    gav :: !Money, -- GAV
    currency :: !Text, -- Valuta
    country :: !Text, -- Land
    isin :: !Text, -- ISIN
    market :: !Text, -- Marknad
    instrumentType :: !Text -- Typ (e.g. "STOCK", "FUND")
  }
  deriving (Show, Eq)

expectedPositionHeaders :: [BSU.ByteString]
expectedPositionHeaders =
  map
    BSU.fromString
    [ "Kontonummer",
      "Namn",
      "Kortnamn",
      "Volym",
      "Marknadsvärde",
      "GAV (SEK)",
      "GAV",
      "Valuta",
      "Land",
      "ISIN",
      "Marknad",
      "Typ"
    ]

instance FromNamedRecord Position where
  parseNamedRecord r = do
    account <- r .: "Kontonummer"
    name <- r .: "Namn"
    shortName <- r .: "Kortnamn"
    volumeRaw <- r .: "Volym"
    -- "Marknadsvärde" contains a non-ASCII character, so the column name must be
    -- built as UTF-8 bytes rather than via OverloadedStrings (which would pack
    -- it as Latin-1 and fail to match the header).
    marketValueRaw <- r .: BSU.fromString "Marknadsvärde"
    gavSekRaw <- r .: "GAV (SEK)"
    gavRaw <- r .: "GAV"
    currency <- r .: "Valuta"
    country <- r .: "Land"
    isin <- r .: "ISIN"
    market <- r .: "Marknad"
    instrumentType <- r .: "Typ"

    volume <- parseField "Volym" parseSvDouble volumeRaw
    marketValue <- parseField "Marknadsvärde" parseSvMoney marketValueRaw
    gavSek <- parseField "GAV (SEK)" parseSvMoney gavSekRaw
    gav <- parseField "GAV" parseSvMoney gavRaw

    return Position {..}
    where
      parseField column parse raw =
        maybe
          (fail $ "Failed to parse \"" <> column <> "\" from value: " <> show raw)
          return
          (parse raw)
