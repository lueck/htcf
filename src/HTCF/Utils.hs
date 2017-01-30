module HTCF.Utils
  ( readIntMaybe
  , readBase
  , readBasePrefixed
  , maybeFun
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Numeric as N
import Data.Char (ord, digitToInt, isDigit)
import Data.List
import Data.Maybe

import HTCF.ConfigParser

-- * Parse a string representation of a number into an integer.

-- | Read a maybe string representation of an decimal number into a
-- maybe integer.
readIntMaybe :: Maybe String -> Maybe Int
readIntMaybe Nothing = Nothing
readIntMaybe (Just s) = fmap fst $ C.readInt $ C.pack s
{-# INLINE readIntMaybe #-}

-- | Reads a number form another base represented in an input string
-- and returns a maybe integer.
--
-- >>> readBase 16 "ff"
-- Just 255
-- >>> readBase 16 "fail"
-- Nothing
readBase :: Int        -- ^ the base
         -> String     -- ^ the string representation
         -> Maybe Int  -- ^ a maybe integer value is returned
readBase bs str
  | null num = Nothing
  | not $ null $ snd $ head num = Nothing
  | otherwise = Just $ fst $ head num
  where
    letterToNum :: Char -> Int
    letterToNum d
      | isDigit d = digitToInt d
      | otherwise = ord d - ord 'a' + 10
    isValidDigit :: Char -> Bool
    isValidDigit d = 0 <= n && n <= bs
      where n = letterToNum d
    num = N.readInt bs isValidDigit letterToNum str
{-# INLINE readBase #-}

-- | Remove the prefix part and then read a string representation a
-- numeric value in another base into a maybe integer value. The base
-- and the method of unprefixing is read from the configuration given
-- as first parameter. Cf. 'TcfIdBase' and 'TcfIdUnprefixMethod'.
readBasePrefixed :: [Config]    -- ^ the configuration
                 -> String      -- ^ the string representation
                 -> Maybe Int   -- ^ Return value
readBasePrefixed cfg str
  | method == Delimiter = readBase bs $ drop (fromMaybe 0 $ fmap (+1) $ elemIndex pxd str) str
  | otherwise = readBase bs $ drop pxl str
  where
    method = getTcfIdUnprefixMethod cfg
    bs = getTcfIdBase cfg
    pxd = getTcfIdPrefixDelimiter cfg
    pxl = getTcfIdPrefixLength cfg
{-# INLINE readBasePrefixed #-}

-- * Other

-- | Return a default function when a Maybe value is Nothing or a
-- closure of the Just value applied to a second function.
--
-- Example:
-- maybeFun id setConfigIdBase (readIntMaybe baseAsString)  
maybeFun :: (a -> a)        -- ^ default function
         -> (b -> a -> a)   -- ^ function where maybe value can be supplied
         -> Maybe b         -- ^ maybe value
         -> (a -> a)        -- ^ returned function
--maybeFun :: forall t t1. t -> (t1 -> t) -> Maybe t1 -> t
maybeFun defaultFun justFun maybee
  | isNothing maybee = defaultFun
  | otherwise = justFun $ fromJust maybee
