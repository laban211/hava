module Features.GroupByCompany.GroupByCompanyTypes (SortableColumn (..), GroupByCompanyContentRow (..), GroupByCompanySortOption (..)) where

import Data.Text (Text)
import Types.Money (Money (..))
import Types.Table.SortOption (SortOption (..), SortOrder (..))

data SortableColumn
  = Company
  | Bought
  | Sold
  | CurrentAmount
  | Profit
  | Dividend
  | ProfitForSold
  deriving (Show, Read, Enum, Bounded, Eq, Ord)

newtype GroupByCompanySortOption = GroupByCompanySortOption {unGroupByCompanySortOption :: SortOption SortableColumn}

-- columns: |Företag|Köpt|Sålt|Nuv. balans|Vinst (kr)|Utdelning (kr)| Vinst sålda (kr)|
data GroupByCompanyContentRow = GroupByCompanyContentRow
  { company :: !Text,
    bought :: !Int,
    sold :: !Int,
    currentAmmount :: !Int,
    profit :: !Money,
    dividence :: !Money,
    profitForSold :: !Money
  }
