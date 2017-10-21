module HTCF.Utils
  ( readIntMaybe
  , readBase
  , parseIDs
  , writeBase
  , idToBase
  , maybeFun
  , guessBase
  , commonPrefix
  , collectIds
  , maybeToField
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Numeric as N
import Data.Char (ord, digitToInt, isDigit, intToDigit, chr)
import Data.Maybe
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Data.ByteString as B
import qualified Data.Csv as Csv

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

-- | Parse the IDs in String to a list of integers of a given base,
-- the length of the prefix is stripped.
parseIDs :: Int -> Int -> String -> [Int]
parseIDs pfxLen base toks =
  mapMaybe ((readBase base) . (drop pfxLen)) $ words toks
{-# INLINE parseIDs #-}

-- | Show an integer number in another base as string.
writeBase :: Int    -- ^ the base
          -> Int    -- ^ the integer number to show in base
          -> String -- ^ returned string representation of integer in base
writeBase base num =
  N.showIntAtBase base numToLetter num ""
  where
    numToLetter n
      | n < 10 = intToDigit n
      | otherwise = chr (ord 'a' + n - 10)
{-# INLINE writeBase #-}

-- | Make an identifier with prefix pfx in base from a Maybe Int
idToBase :: String -> Int -> Maybe Int -> Maybe String
idToBase pfx base = fmap ((pfx++) . (writeBase base))
{-# INLINE idToBase #-}


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

-- | guess the base from a list of string encoded numbers by getting
-- the count of different tokens. Take care to pass enough data!
guessBase :: [[Char]] -> Int
guessBase =
  length . (foldl (\acc x -> if (x `elem` acc) then acc else (x:acc)) []) . concat

-- | Get the common prefix of a list of strings (or lists in general).
commonPrefix :: (Eq a) => [[a]] -> [a]
commonPrefix = foldl1 commonPrefixPair

commonPrefixPair :: (Eq e) => [e] -> [e] -> [e]
commonPrefixPair _ [] = []
commonPrefixPair [] _ = []
commonPrefixPair (x:xs) (y:ys)
  | x == y    = x : commonPrefixPair xs ys
  | otherwise = []

-- | Collect IDs from a subtree.
collectIds :: XmlTree -> [String]
collectIds (XN.NTree n cs)
  | XN.isElem n = (fromMaybe [] $ fmap (concatMap collectIds) $ XN.getAttrl n) ++ (concatMap collectIds cs)
  | XN.isAttr n && (XN.getLocalPart n == Just "id") = (fromMaybe "" $ XN.getText $ head cs):[]
  | otherwise = concatMap collectIds cs

-- | Convert maybe to csv, with default when Nothing.
maybeToField :: (Csv.ToField a) => B.ByteString -- ^ Default value
         -> Maybe a -- ^ the maybe field, 
         -> Csv.Field
maybeToField deflt f = maybe deflt Csv.toField f 
