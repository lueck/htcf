module HTCF.Abbrev
  ( runAbbrevParser
  ) where

import System.IO
import System.Directory
import System.Environment

-- | Each line of the abbreviations file is an abbreviation, just the
-- token without dot. 'runAbbrevParser' returns a list of Strings
-- generated from the lines.
runAbbrevParser :: FilePath -> IO [String]
runAbbrevParser fname = do
  exists <- doesFileExist fname
  progName <- getProgName
  if exists then
    do { results <- readFile fname
       ; return $ lines results}
    else
    do { hPutStrLn stderr (progName ++ ": No abbreviations file found")
       ; return [] }
