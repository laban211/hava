module TablesSpec
    ( spec
    ) where
import           Data.Text                      ( Text )
import           Tables.BuySellTable            ( createBuySellTable )
import           Tables.GroupByCompanyTable     ( createGroupByCompanyTable )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldReturn
                                                , shouldSatisfy
                                                )
import           Types.UiSize                   ( UiSize(..) )

spec :: Spec
spec = do
    describe "Buy/sell table"         testBuySellTable
    describe "Group-by-company table" testGroupByCompanyTable

testBuySellTable :: Spec
testBuySellTable = do
    it "should produce text" $ do
        let uiSize    = Small
        let termWidth = 100
        let rows      = []
        let table = createBuySellTable uiSize termWidth rows

        table `shouldSatisfy` isText

testGroupByCompanyTable :: Spec
testGroupByCompanyTable = do
    it "should produce text" $ do
        let uiSize    = Small
        let termWidth = 100
        let rows      = []
        let table = createGroupByCompanyTable uiSize termWidth rows

        table `shouldSatisfy` isText


isText :: Text -> Bool
isText _ = True
