module HTCF.WriteTcf
  ( runTcfWriter
  ) where

import Text.XML.HXT.Core

import HTCF.ConfigParser
import HTCF.TokenLayer
import HTCF.TextLayer

-- Does not really make sense here. Put it in the command line
-- program.
runTcfWriter :: [Config] -> [{- FIXME: -}Token] -> IO (String)
runTcfWriter cfg toks = do
  rc <- runX (root [] [(writeTokenLayer cfg toks)] >>>
              writeDocumentToString [withIndent yes])
  return $ concat rc
