{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.UtilTypes
  ( parseInt
  , parseDouble
  , parseMoney
  , parseMoneyDefault0
  , SortedByDateList(..)
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Read                as TR
import           GHC.Exts                       ( IsList(..) )
import           Prelude
import           Types.Money                    ( Money(Money) )

-- todo: not sure where to put these
parseInt :: Text -> Maybe Int
parseInt t = case TR.signed TR.decimal t of
  Right (n, _) -> Just n
  _            -> Nothing

parseDouble :: Text -> Maybe Double
parseDouble t = case TR.double t of
  Right (d, _) -> Just d
  _            -> Nothing

parseMoney :: Text -> Maybe Money
parseMoney t = case parseDouble t of
  Just d  -> Just $ Money d
  Nothing -> Nothing

parseMoneyDefault0 :: Text -> Money
parseMoneyDefault0 t = case parseMoney t of
  Just m  -> m
  Nothing -> Money 0.0

newtype SortedByDateList a = SortedByDateList{ getSortedByDateList :: [a] }
  deriving (Show, Eq, Ord, Functor, Foldable)

instance IsList (SortedByDateList a) where
  type Item (SortedByDateList a) = a
  fromList = SortedByDateList
  toList   = getSortedByDateList
