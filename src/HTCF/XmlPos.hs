{-# LANGUAGE FlexibleContexts #-}
module HTCF.XmlPos where

import Text.Parsec

import HTCF.TokenLayer

type TokenWithPos = (Maybe SourcePos, Maybe SourcePos, String)


-- | A character that *allows* token boundary.
tokenDelim :: Stream s m Char => ParsecT s u m Char
tokenDelim = space <|> oneOf ".!,;" -- FIXME:

tokenDelims :: Stream s m Char => ParsecT s u m ()
tokenDelims = skipMany tokenDelim

nonContent :: Stream s m Char => ParsecT s u m ()
nonContent =
  try comment <|> try proci <|> try decl <|> try closeTag <|> try emptyTag <|> try openTag
  --comment <|> proci <|> decl <|> closeTag <|> emptyTag <|> openTag

openTag :: Stream s m Char => ParsecT s u m ()
openTag = do
  char '<'
  manyTill (noneOf "<") (try (char '>'))
  return ()

closeTag :: Stream s m Char => ParsecT s u m ()
closeTag = do
  string "</"
  manyTill (noneOf "<") (try (char '>'))
  return ()

emptyTag :: Stream s m Char => ParsecT s u m ()
emptyTag = do
  char '<'
  manyTill (noneOf "<") (try (string "/>"))
  return ()

comment :: Stream s m Char => ParsecT s u m ()
comment = do
  string "<!--"
  manyTill anyChar (try (string "-->"))
  return ()

decl :: Stream s m Char => ParsecT s u m ()
decl = do
  string "<?xml"
  manyTill (noneOf "<") (try (string "?>"))
  return ()

proci :: Stream s m Char => ParsecT s u m ()
proci = do
  string "<?"
  manyTill (noneOf "<") (try (string "?>"))
  return ()

-- | Match a token. Returns a triple of start position, end position and token.
--
-- >>> parse (matchToken Nothing Nothing "Hallo") "(stdio)" "<em>Hal<br/>lo</em> "
-- Right (Just "(stdio)" (line 1, column 5),Just "(stdio)" (line 1, column 14),"Hallo")
--
-- >>> parse (matchToken Nothing Nothing "Hallo") "(stdio)" "<em>Hal<br/>lo"
-- Left "(stdio)" (line 1, column 15):
-- unexpected end of input
-- expecting "<!--", "<?", "<?xml", "</", "<" or space
--
-- >>> parse (matchToken Nothing Nothing "Dr.") "(stdio)" "Dr. "
-- Right (Just "(stdio)" (line 1, column 1),Just "(stdio)" (line 1, column 3),"Dr.")
--
-- >>> parse (matchToken Nothing Nothing "Dr") "(stdio)" "Dr. "
-- Right (Just "(stdio)" (line 1, column 1),Just "(stdio)" (line 1, column 2),"Dr")
matchToken :: Stream s m Char
              => Maybe SourcePos -- ^ start pos
           -> Maybe SourcePos    -- ^ last matched character
           -> String             -- ^ the token
           -> ParsecT s u m TokenWithPos
matchToken (Just start) (Just end) [] = do
  skipMany nonContent
  lookAhead tokenDelim -- look ahead for delimiter, but do not consume.
  return (Just start, Just end, [])
matchToken Nothing Nothing tok@(x:xs) = do
  skipMany nonContent
  pos <- getPosition
  char x
  (_, end, _) <- matchToken (Just pos) (Just pos) xs
  return (Just pos, end, tok)
matchToken start _ tok@(x:xs) = do
  skipMany nonContent
  pos <- getPosition
  char x
  (_, end, _) <- matchToken start (Just pos) xs
  return (start, end, tok) 
