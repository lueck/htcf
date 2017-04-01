{-# LANGUAGE GADTs #-}
module HTCF.Range
  ( PostgresRange (..)
  ) where

-- | Wrapper for data types that have fields that must be exported as
-- PostgreSQL's range types.
data PostgresRange a where
  PostgresRange :: a -> PostgresRange a


