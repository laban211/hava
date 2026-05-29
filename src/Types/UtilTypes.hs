{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Types.UtilTypes
  ( parseInt,
    parseDouble,
    parseMoney,
    parseMoneyDefault0,
    normalizeSvNumber,
    parseSvDouble,
    parseSvMoney,
    SortedByDateList (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import GHC.Exts (IsList (..))
import Types.Money (Money (Money))
import Prelude

-- todo: not sure where to put these
parseInt :: Text -> Maybe Int
parseInt t = case TR.signed TR.decimal t of
  Right (n, _) -> Just n
  _ -> Nothing

parseDouble :: Text -> Maybe Double
parseDouble t = case TR.double t of
  Right (d, _) -> Just d
  _ -> Nothing

parseMoney :: Text -> Maybe Money
parseMoney t = case parseDouble t of
  Just d -> Just $ Money d
  Nothing -> Nothing

parseMoneyDefault0 :: Text -> Money
parseMoneyDefault0 t = case parseMoney t of
  Just m -> m
  Nothing -> Money 0.0

-- Avanza's "consolidated holdings" export formats numbers in the Swedish locale:
-- a comma as the decimal separator and (for large values) spaces as thousands
-- separators, e.g. "1 234 567,89". This normalizes such a value into something
-- 'parseDouble' understands by dropping the (possibly non-breaking) spaces and
-- swapping the decimal comma for a dot.
normalizeSvNumber :: Text -> Text
normalizeSvNumber =
  T.replace (T.pack ",") (T.pack ".")
    . T.filter (\c -> c /= ' ' && c /= '\160')

parseSvDouble :: Text -> Maybe Double
parseSvDouble = parseDouble . normalizeSvNumber

parseSvMoney :: Text -> Maybe Money
parseSvMoney t = Money <$> parseSvDouble t

newtype SortedByDateList a = SortedByDateList {getSortedByDateList :: [a]}
  deriving (Show, Eq, Ord, Functor, Foldable)

instance IsList (SortedByDateList a) where
  type Item (SortedByDateList a) = a
  fromList = SortedByDateList
  toList = getSortedByDateList
