module Features.GroupByCompany.GroupByCompanyArgParse (ParsedGroupBuySellCliFlags (..), parseCliFlags, defaultGroupByCompanySortOption, allSortableColumnsKeys, sortableColumnToStr) where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Text as T
import Features.GroupByCompany.GroupByCompanyTypes
import Text.Read (readMaybe)
import Types.Table.SortOption (SortOption (..), SortOrder (..), allSortKeys, parseOrder)

data ParsedGroupBuySellCliFlags = ParsedGroupBuySellCliFlags
  { sortOptions :: !GroupByCompanySortOption,
    limit :: !(Maybe Int)
  }

defaultFlags :: ParsedGroupBuySellCliFlags
defaultFlags =
  ParsedGroupBuySellCliFlags
    { sortOptions = defaultGroupByCompanySortOption,
      limit = Nothing
    }

defaultGroupByCompanySortOption :: GroupByCompanySortOption
defaultGroupByCompanySortOption = GroupByCompanySortOption (SortOption Company Descending)

parseCliFlags :: [String] -> Either String ParsedGroupBuySellCliFlags
parseCliFlags args = go args defaultFlags
  where
    go [] acc = Right acc
    go ("--sort" : col : ord : rest) acc =
      case parseGroupByCompanySortOpts (col ++ " " ++ ord) of
        Right opt -> go rest acc {sortOptions = opt}
        Left err -> Left $ "Failed to parse --sort flag: " ++ err
    go ("--sort" : col : rest) acc =
      case parseGroupByCompanySortOpts col of
        Right opt -> go rest acc {sortOptions = opt}
        Left err -> Left $ "Failed to parse --sort flag: " ++ err
    go ("--limit" : num : rest) acc =
      case parseLimit num of
        Just opt -> go rest acc {limit = Just opt}
        Nothing ->
          Left $
            "Failed to parse --limit flag due to invalid value: \""
              ++ num
              ++ "\". Expected an integear larger than 1"

parseGroupByCompanySortOpts :: String -> Either String GroupByCompanySortOption
parseGroupByCompanySortOpts input =
  case words (map toLower input) of
    [col, ord] -> do
      column <- parseCol col
      order <- parseOrd ord
      Right $ GroupByCompanySortOption (SortOption column order)
    -- Use default ordering
    [col] -> do
      column <- parseCol col
      let order' = order (unGroupByCompanySortOption defaultGroupByCompanySortOption)
      Right $ GroupByCompanySortOption (SortOption column order')
    _ -> Left "Invalid input format. Use: <column> <order> (e.g., 'profit desc')."
  where
    parseCol col =
      maybe
        ( Left $
            "Invalid column \""
              ++ col
              ++ "\". Please use one of the supported options: "
              ++ intercalate ", " allSortableColumnsKeys
        )
        Right
        $ parseSortableColumnMap col

    parseOrd ord =
      maybe
        ( Left $
            "Invalid order: \""
              ++ ord
              ++ "\". Please use one of the support options: "
              ++ intercalate ", " allSortKeys
        )
        Right
        (parseOrder ord)

parseLimit :: String -> Maybe Int
parseLimit str = do
  n <- readMaybe str
  if n > 0 then Just n else Nothing

strToSortableColumnMap :: Map.Map String SortableColumn
strToSortableColumnMap =
  Map.fromList $
    map (\col -> (lowercaseFirst (show col), col)) [minBound .. maxBound]

parseSortableColumnMap :: String -> Maybe SortableColumn
parseSortableColumnMap key = Map.lookup key strToSortableColumnMap

sortableColumnToStr :: SortableColumn -> String
sortableColumnToStr col = lowercaseFirst (show col)

allSortableColumnsKeys :: [String]
allSortableColumnsKeys = Map.keys strToSortableColumnMap

lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x : xs) = toLower x : xs