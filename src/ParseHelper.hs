{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module ParseHelper
  ( processData
  , AvanzaRow
  , printableMoney
  , mult
  , dive
  ) where

-- bytestring
import           Data.ByteString.Char8         as BS
                                                ( ByteString
                                                , pack
                                                )
import           Data.ByteString.Lazy          as BSL
                                                ( drop
                                                , take
                                                , toStrict
                                                )
import qualified Data.ByteString.Lazy          as ByteString
                                         hiding ( putStrLn )
import           Data.ByteString.Lazy.Char8    as BSL
                                                ( pack
                                                , unpack
                                                )
import           Data.ByteString.Lazy.Search   as BSL
                                                ( )
import           Data.ByteString.Lazy.UTF8     as BLU
                                                ( fromString )
import           Data.ByteString.Search        as BS
                                                ( replace )
import           Data.ByteString.UTF8          as BU
                                                ( fromString )
-- other
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Csv                       ( (.:)
                                                , DecodeOptions(decDelimiter)
                                                , Field
                                                , FromField(..)
                                                , FromNamedRecord(..)
                                                , Parser
                                                , decodeByNameWith
                                                , defaultDecodeOptions
                                                )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Data.Text                     as T
                                                ( pack
                                                , singleton
                                                )
import           Data.Text.Encoding             ( )
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import           GHC.TypeLits                   ( Symbol )
import           Text.Read                      ( readMaybe )

-- Types
import           Types.AvanzaBuySellRow         ( AvanzaBuySellRow )
import           Types.AvanzaRow                ( AvanzaRow(..) )
import           Types.Money                    ( Money(..) )

-- todo: move
mult :: Money -> Double -> Money
mult x y = Money $ unMoney x * y

dive :: Money -> Double -> Money
dive x y = Money $ unMoney x * y

instance FromNamedRecord AvanzaRow where
  parseNamedRecord r =
    AvanzaRow
      <$> r
      .:  "Datum"
      <*> r
      .:  "Konto"
      <*> r
      .:  "Typ av transaktion"
      <*> r
      .:  BU.fromString "Värdepapper/beskrivning"
      <*> r
      .:  "Antal"
      <*> r
      .:  "Kurs"
      <*> r
      .:  "Belopp"
      <*> r
      .:  "Courtage"
      <*> r
      .:  "Valuta"
      <*> r
      .:  "ISIN"

instance FromField Money where
  parseField s = parseMoney s

parseMoney :: Field -> Parser Money
parseMoney x =
  BS.replace (BS.pack ",") (BS.pack ".") x & BSL.unpack & read & pure

processData :: ByteString.ByteString -> [AvanzaRow]
processData csvData = case decodeByNameWith decOptions (dropBOM csvData) of
  Left  err    -> error err
  Right (h, v) -> V.toList v
 where
  decOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

dropBOM :: ByteString.ByteString -> ByteString.ByteString
dropBOM x | BSL.take 3 x == BSL.pack (map chr [0xEF, 0xBB, 0xBF]) = BSL.drop 3 x
          | otherwise = x

printableMoney :: Maybe Money -> Text
printableMoney = T.pack . maybe "-" show
