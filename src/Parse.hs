{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse
  ( parseCsvData,
    parsePositionCsvData,
  )
where

-- bytestring
import qualified Data.ByteString as BS -- strict bytes (Word8 API)
import qualified Data.ByteString.Lazy as BSL -- lazy bytes
import qualified Data.ByteString.UTF8 as BSU -- UTF-8 <-> String helpers
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
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word (Word8)
import System.Exit (die)
-- Types
import Types.Money (Money (..))
import Types.Position (Position, expectedPositionHeaders)
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
    Transaction (..),
  )

expectedTransactionHeaders :: [BSU.ByteString]
expectedTransactionHeaders =
  map
    BSU.fromString
    [ "Datum",
      "Konto",
      "Typ av transaktion",
      "Värdepapper/beskrivning",
      "Antal",
      "Kurs",
      "Belopp",
      "Courtage",
      "Instrumentvaluta",
      "ISIN"
    ]

instance FromNamedRecord Transaction where
  parseNamedRecord r = do
    date <- r .: "Datum"
    account <- r .: "Konto"
    action <- r .: "Typ av transaktion"
    company <- r .: BSU.fromString "Värdepapper/beskrivning"
    quantity <- r .: "Antal"
    rate <- r .: "Kurs"
    amount <- r .: "Belopp"
    courtage <- r .: "Courtage"
    currency <- r .: "Instrumentvaluta"
    isin <- r .: "ISIN"

    return GenericTransaction {..}

-- Note: "resultat" avilable but not parsed at this time

parseCsvData :: BSL.ByteString -> [Transaction]
parseCsvData = parseCsvWith expectedTransactionHeaders

parsePositionCsvData :: BSL.ByteString -> [Position]
parsePositionCsvData = parseCsvWith expectedPositionHeaders

-- | Decode a ';'-delimited Avanza CSV export into a list of records of any
--   format with a 'FromNamedRecord' instance. Strips a leading UTF-8 BOM and
--   validates the header against the format's expected columns before decoding.
parseCsvWith ::
  (FromNamedRecord a) => [BSU.ByteString] -> BSL.ByteString -> [a]
parseCsvWith expectedHeaders csvData =
  let withoutBom = dropBOM csvData
      header = readHeader withoutBom
      decOptions = defaultDecodeOptions {decDelimiter = fromIntegral (ord ';')}
   in case checkHeadersStrict expectedHeaders header of
        Just msg -> error msg
        Nothing -> case decodeByNameWith decOptions withoutBom of
          Left err -> error err
          Right (h, v) -> V.toList v

dropBOM :: BSL.ByteString -> BSL.ByteString
dropBOM x
  | BSL.take 3 x == BSL.pack (map (fromIntegral . fromEnum) ['\239', '\187', '\191']) = BSL.drop 3 x
  | otherwise = x

readHeader :: BSL.ByteString -> [BS.ByteString]
readHeader bs =
  let (hdr, _) = BSL.break (== nl) bs
   in BS.split semi (BSL.toStrict hdr)

checkHeadersStrict :: [BS.ByteString] -> [BS.ByteString] -> Maybe String
checkHeadersStrict expected found =
  if all (`elem` found) expected
    then Nothing
    else
      let missing = filter (`notElem` found) expected
          extra = filter (`notElem` expected) found
          pretty = L.intercalate ", "
          s = map BSU.toString
       in Just $
            unlines
              [ "Unexpected CSV header.",
                "Missing: " <> pretty (s missing),
                "Extra:   " <> pretty (s extra),
                "Expected headers:\n" <> unlines (map ("  " <>) (s expected)),
                "Found headers:\n" <> unlines (map ("  " <>) (s found))
              ]

-- Delimiters as Word8
semi, nl :: Word8
semi = 59 -- ';'
nl = 10 -- '\n'
