module HTCF.PosParser.ParserInterface
where

import Control.Arrow.ArrowList

import Text.XML.HXT.DOM.Interface
import Text.XML.HXT.Arrow.XmlArrow

import qualified HTCF.PosParser.XmlParsec           as XP

parseXmlDoc                     :: ArrowXml a => a (String, String) XmlTree
parseXmlDoc                     =  arr2L XP.parseXmlDocument

xreadCont                       :: ArrowXml a => a String XmlTree
xreadCont                       =  arrL XP.xread

xreadDoc                        :: ArrowXml a => a String XmlTree
xreadDoc                        =  arrL XP.xreadDoc

