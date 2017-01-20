{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe

import Text.XML.TCF.Parser.ConfigParser
import Text.XML.TCF.Parser.TcfLayerParser
import Text.XML.TCF.Parser.TcfElement
import Text.XML.TCF.Parser.Tokenizer

data Convert =
  Convert { configFile :: Maybe String
          , outputMethod :: Maybe OutputMethod
          , outputStructure :: Bool
          , inFile :: String
          }

data OutputMethod = Raw | PrettyList

convert_ :: Parser Convert
convert_ = Convert
  <$> optional (strOption (short 'c'
                            <> long "config"
                            <> help "Specify a config file."
                            <> metavar "CONFIGFILE" ))
  <*> optional ((flag' Raw (short 'r'
                             <> long "raw"
                             <> help "Output in raw format."))
                <|>
                (flag' PrettyList (short 'u'
                                    <> long "human"
                                    <> help "Output formatted human readable.")))
  <*> flag True False (short 'S'
                       <> long "-no-structure"
                       <> help "Do not output structure layer.")
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile outputMethod outputStructure fName) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  parsed <- runX (readDocument [withValidate no] fName >>>
                  propagateNamespaces //>
                  hasName "text" >>>
                  multi parserArrow
                 )
  writeOut parsed
  where writeOut = case outputMethod of
          Just Raw -> print . propagateOffsets
          Just PrettyList -> putStrLn . concatMap serialize . propagateOffsets
          otherwise -> print . tokenize . propagateOffsets
        parserArrow
          | outputStructure = mkTcfElement
          | otherwise = (isText >>> mkTcfText)

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Convert an XML file to TCF, the Text Corpus Format."
            <> header "xml2tcf - convert an XML file to TCF." )
