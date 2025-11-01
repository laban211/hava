module ParseSpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Parse as P
import Test.Hspec
import Types.Transaction.GenericTransaction (GenericTransaction (..))
import Types.Transaction.TransactionBuySell (transformToBuySell)
import Control.Exception (evaluate, ErrorCall (ErrorCall))
import Data.List (isInfixOf)
import TestUtils (shouldThrowErrorContaining)

spec :: Spec
spec = do
  describe "test buy transaction" testParseBuy
  describe "test basic parsing" testBasicParsing

testBasicParsing :: Spec
testBasicParsing = do
  it "should pupulate the expected fields for GenericTransaction data type" $ do
    -- Populate with header row twice so that the header will be pared into
    -- Generic transaction. We can then make sure the values get parsed into
    -- the expected fields
    let row = stringsToCsvByteString [csvHeader2025, csvHeader2025]
    let expected =
          [ GenericTransaction
              { date = T.pack "Datum",
                account = T.pack "Konto",
                action = T.pack "Typ av transaktion",
                company = T.pack "Värdepapper/beskrivning",
                quantity = T.pack "Antal",
                rate = T.pack "Kurs",
                amount = T.pack "Belopp",
                courtage = T.pack "Courtage",
                currency = T.pack "Instrumentvaluta",
                isin = T.pack "ISIN"
              }
          ]
    P.parseCsvData row `shouldBe` expected

  it "fails with a friendly message if a required header is missing" $ do
    let badHeader =
          "Datum-with-extra-chars;Konto;Typ av transaktion;Värdepapper/beskrivning;Antal;Kurs;Belopp;Courtage;Instrumentvaluta;ISIN"
    let csv = stringsToCsvByteString [badHeader] 

    evaluate (P.parseCsvData csv) 
      `shouldThrowErrorContaining` "Unexpected CSV header." 

    evaluate (P.parseCsvData csv)
      `shouldThrowErrorContaining` "Missing: Datum"

    evaluate (P.parseCsvData csv)
       -- Note: 2 extra spaces is expected after "Extra" to align with "Missing"
      `shouldThrowErrorContaining` "Extra:   Datum-with-extra-chars" 

testParseBuy :: Spec
testParseBuy = do
  it "should parse buy transaction" $ do
    let withBuyRow =
          stringsToCsvByteString
            [ csvHeader2025,
              "2024-01-01;1111111;Köp;Company A;5;100;-500;SEK;7;;SEK;some-isin;"
            ]
    let expectedGenericTransaction =
          [ GenericTransaction
              { date = T.pack "2024-01-01",
                account = T.pack "1111111",
                action = T.pack "Köp",
                company = T.pack "Company A",
                quantity = T.pack "5",
                rate = T.pack "100",
                amount = T.pack "-500",
                courtage = T.pack "7",
                currency = T.pack "SEK",
                isin = T.pack "some-isin"
              }
          ]
    let parsedGenericTransaction = P.parseCsvData withBuyRow
    parsedGenericTransaction `shouldBe` expectedGenericTransaction

    -- should be good to parse into the buy/sell-type
    let parsedBuySellTransaction = transformToBuySell $ head parsedGenericTransaction
    parsedBuySellTransaction `shouldSatisfy` isJust

csvHeader :: String
csvHeader = "Datum;Konto;Typ av transaktion;Värdepapper/beskrivning;Antal;Kurs;Belopp;Courtage;Valuta;ISIN"

csvHeader2024 :: String
csvHeader2024 = "Datum;Konto;Typ av transaktion;Värdepapper/beskrivning;Antal;Kurs;Belopp;Transaktionsvaluta;Courtage (SEK);Valutakurs;Instrumentvaluta;ISIN;Resultat"

csvHeader2025 :: String
csvHeader2025 = "Datum;Konto;Typ av transaktion;Värdepapper/beskrivning;Antal;Kurs;Belopp;Transaktionsvaluta;Courtage;Valutakurs;Instrumentvaluta;ISIN;Resultat"

stringsToCsvByteString :: [String] -> UTF8.ByteString
stringsToCsvByteString rows = UTF8.fromString $ unlines rows
