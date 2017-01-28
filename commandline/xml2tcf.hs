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
import Text.XML.TCF.Arrow.ArrowXml

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
                  propagateNamespaces >>>
                  stripQNames (getDroppedTrees config) //>
                  hasName "text" >>>
                  multi (parserArrow config)
                 )
  writeOut (config, parsed)
  where
    writeOut :: ([Config], [TcfElement]) -> IO ()
    writeOut = case outputMethod of
          Just Raw -> print . (uncurry (\_ -> propagateOffsets))
          Just PrettyList -> putStrLn . concatMap serialize . (uncurry (\_ -> propagateOffsets))
          otherwise -> print . (uncurry (\cfg -> tokenize cfg . propagateOffsets))
    tokWithOffsets cfg p = tokenize cfg $ propagateOffsets p
    parserArrow
      | outputStructure = mkTcfElement
      | otherwise = mkTcfElementButStructure

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Convert an XML file to TCF, the Text Corpus Format."
            <> header "xml2tcf - convert an XML file to TCF." )
