{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseCsvData,
  )
where

-- bytestring
import Data.ByteString.Char8 as BS (ByteString, pack)
import Data.ByteString.Lazy as BSL
  ( drop,
    take,
    toStrict,
  )
import qualified Data.ByteString.Lazy as ByteString hiding
  ( putStrLn,
  )
import Data.ByteString.Lazy.Char8 as BSL (pack, unpack)
import Data.ByteString.Lazy.Search as BSL ()
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.ByteString.Search as BS (replace)
import Data.ByteString.UTF8 as BU (fromString)
-- other
import Data.Char (chr, ord)
import Data.Csv
  ( DecodeOptions (decDelimiter),
    Field,
    FromField (..),
    FromNamedRecord (..),
    Parser,
    decodeByNameWith,
    defaultDecodeOptions,
    (.:),
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
-- Types
import Types.Money (Money (..))
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
    Transaction (..),
  )

instance FromNamedRecord Transaction where
  parseNamedRecord r = do
    date      <- r .: "Datum"
    account   <- r .: "Konto"
    action    <- r .: "Typ av transaktion"
    company   <- r .: BU.fromString "Värdepapper/beskrivning"
    quantity  <- r .: "Antal"
    rate      <- r .: "Kurs"
    amount    <- r .: "Belopp"
    courtage  <- r .: "Courtage"
    currency  <- r .: "Instrumentvaluta"
    isin      <- r .: "ISIN"

    return GenericTransaction {..}

-- Note: "resultat" avilable but not parsed at this time

parseCsvData :: ByteString.ByteString -> [Transaction]
parseCsvData csvData = case decodeByNameWith decOptions (dropBOM csvData) of
  Left err -> error err
  Right (h, v) -> V.toList v
  where
    decOptions = defaultDecodeOptions {decDelimiter = fromIntegral (ord ';')}

dropBOM :: ByteString.ByteString -> ByteString.ByteString
dropBOM x
  | BSL.take 3 x == BSL.pack (map chr [0xEF, 0xBB, 0xBF]) = BSL.drop 3 x
  | otherwise = x
