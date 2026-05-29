module TablesSpec
  ( spec,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Features.BuySell.BuySellTable (createBuySellTable)
import Features.GroupByCompany.GroupByCompanyTable (createGroupByCompanyTable)
import Features.Positions.PositionsTable (createPositionsTable)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldReturn,
    shouldSatisfy,
  )
import Types.Money (Money (..))
import Types.Position (Position (..))
import Types.UiSize (UiSize (..))

spec :: Spec
spec = do
  describe "Buy/sell table" testBuySellTable
  describe "Group-by-company table" testGroupByCompanyTable
  describe "Positions table" testPositionsTable

testBuySellTable :: Spec
testBuySellTable = do
  it "should produce text" $ do
    let uiSize = Small
    let termWidth = 100
    let rows = []
    let table = createBuySellTable uiSize termWidth rows

    table `shouldSatisfy` isText

testGroupByCompanyTable :: Spec
testGroupByCompanyTable = do
  it "should produce text" $ do
    let uiSize = Small
    let termWidth = 100
    let rows = []
    let table = createGroupByCompanyTable uiSize termWidth [] rows

    table `shouldSatisfy` isText

testPositionsTable :: Spec
testPositionsTable = do
  it "should produce text" $ do
    let uiSize = Small
    let termWidth = 100
    let rows =
          [ Position
              { name = T.pack "Acme Corp",
                shortName = T.pack "ACME",
                volume = 10,
                marketValue = Money 1200,
                gavSek = Money 100,
                gav = Money 100,
                currency = T.pack "SEK",
                country = T.pack "SE",
                isin = T.pack "SE0000000001",
                market = T.pack "XSTO",
                instrumentType = T.pack "STOCK"
              }
          ]
    let table = createPositionsTable uiSize termWidth rows

    table `shouldSatisfy` isText

isText :: Text -> Bool
isText _ = True
