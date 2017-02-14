module HTCF.LineOffsets
  ( lineOffsets
  , runLineOffsetParser
  ) where
    
import Text.Parsec

-- | The line offset parser.
--
-- The lineOffsets parser returns the offsets of the lines fed to it
-- as a list. This list can be used to calculate offsets from Parsec's
-- SourcePos.
-- Usage:
-- > parse lineOffsets loc str
--
-- > λ> parse lineOffsets "" "ajsdf \n asdjf\r asf\nadsf"
-- > Right [0,7,19]
lineOffsets :: Parsec String () [Int]
lineOffsets = do
  ls <- lineLenths
  return $ init $ scanl (+) 0 ls

lineLenths :: Parsec String () [Int]
lineLenths = do
  l <- lineLenth
  ls <- many $ do
    newline
    lineLenth
  eof
  return $ l:ls

lineLenth :: Parsec String () Int
lineLenth = do
  l <- many $ noneOf "\n"
  return $ length l + 1

-- | Runs the line offset parser in the IO monad.
runLineOffsetParser :: FilePath -> IO [Int]
runLineOffsetParser fName = do
  c <- readFile fName
  return $ either (fail . show) id (parse lineOffsets ("(" ++ fName ++ ")") c)
