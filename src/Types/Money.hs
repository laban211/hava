{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Money
  ( Money (..),
  )
where

newtype Money = Money
  {unMoney :: Double}
  deriving (Eq, Num, Fractional, Ord)
  deriving newtype (Read, Show)
