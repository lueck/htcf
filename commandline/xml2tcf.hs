{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe

import Text.XML.TCF.Parser.ConfigParser
import Text.XML.TCF.Parser.TextLayerParser
import Text.XML.TCF.Parser.TcfElement

data Convert =
  Convert { configFile :: Maybe String
          , inFile :: String
          }

convert_ :: Parser Convert
convert_ = Convert
  <$> optional (strOption (short 'c'
                            <> long "config"
                            <> help "Specify a config file."
                            <> metavar "CONFIGFILE" ))
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile fName) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  textLayer <- runX (readDocument [withValidate no] fName >>>
                     propagateNamespaces //>
                     hasName "text" >>>
                     multi isText >>> mkTcfText
                    )
  print $ propagateOffsets textLayer

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Convert an XML file to TCF, the Text Corpus Format."
            <> header "xml2tcf - convert an XML file to TCF." )
