{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module HTCF.Range
  ( PostgresRange (..)
  , toPgRange
  ) where

import qualified Data.ByteString as B
import qualified Data.Csv as Csv


-- | Wrapper for data types that have fields that must be exported as
-- PostgreSQL's range types.
data PostgresRange a where
  PostgresRange :: a -> PostgresRange a

-- | A function for converting to csv
toPgRange :: (Csv.ToField a) => Maybe a -> Maybe a -> Csv.Field
toPgRange (Just s) (Just e) = B.concat [ "[", (Csv.toField s), ",", (Csv.toField e), "]"]
toPgRange _ _ = ""
  
