{-# LANGUAGE CPP                #-}
module HTCF.PosParser.XmlParsec
    ( charData
    , charData'
    , comment
    , comment'
    , pI
    , pI'
    , cDSect
    , cDSect'
    , document
    , document'
    , element
    , content
    , contentWithTextDecl
    , xread
    , xreadDoc

    , parseXmlContent
    , parseXmlDocument

    , parseXmlPart
    , parseXmlText

    )
where

#if MIN_VERSION_base(4,8,2)
#else
import           Control.Applicative                   ((<$>))
#endif

import           Text.ParserCombinators.Parsec         (between, char, eof,
                                                        getInput, getPosition,
                                                        many, many1,
                                                        notFollowedBy, option,
                                                        runParser, sourceName,
                                                        string, try, unexpected,
                                                        (<?>), (<|>),
                                                        sourceName,
                                                        sourceLine,
                                                        sourceColumn,
                                                        setSourceColumn)

import           Text.XML.HXT.DOM.Interface
import           Text.XML.HXT.DOM.ShowXml              (xshow)
import           Text.XML.HXT.DOM.XmlNode              (changeAttrl,
                                                        getAttrName, getAttrl,
                                                        getChildren, getText,
                                                        isRoot, isText,
                                                        mergeAttrl, mkAttr',
                                                        mkCdata', mkCmt',
                                                        mkDTDElem', mkElement',
                                                        mkError', mkPi',
                                                        mkRoot', mkText')
import           Text.XML.HXT.Parser.XmlCharParser     (SimpleXParser, XPState,
                                                        XParser,
                                                        withNormNewline,
                                                        withoutNormNewline,
                                                        xmlChar)
import qualified Text.XML.HXT.Parser.XmlDTDTokenParser as XD
import qualified Text.XML.HXT.Parser.XmlTokenParser    as XT

import qualified Text.XML.HXT.Parser.XmlParsec as XP
import qualified Text.XML.HXT.DOM.XmlNode as XN

import           Control.FlatSeq

import           Data.Char                             (toLower)
import           Data.Maybe

import Text.Parsec (SourcePos)
import Data.Tree.Class (mkTree)

import HTCF.Position (mkPositionNode, mkPositionAttrs)

-- import Debug.Trace

corPos :: SourcePos -> Int -> SourcePos
corPos p i = setSourceColumn p ((sourceColumn p)+i)

mkPosText :: SourcePos -> SourcePos -> String -> XmlTree
mkPosText start end t =
  mkTree (XN.mkText t) [(mkPositionNode start end)]

mkPosElement :: QName -> SourcePos -> SourcePos -> XmlTrees -> XmlTrees -> XmlTree
mkPosElement n start end al cl =
  id $!! XN.mkElement n (al++(mkPositionAttrs start end)) cl

mkPosCharRef :: Int -> SourcePos -> SourcePos -> XmlTree
mkPosCharRef i start end = mkTree (XN.mkCharRef i) [(mkPositionNode start end)]

-- ------------------------------------------------------------
--
-- Character Data (2.4)

charData                :: XParser s XmlTrees
charData
    = many (charData' <|> referenceT)

charData' :: XParser s XmlTree
charData' =  do
  start <- getPosition
  t <- XT.allBut1 many1 (\ c -> not (c `elem` "<&")) "]]>"
  end <- getPosition
  -- correct position: Start: SourcePos does not know column 0, so
  -- -1. End: The ending char was alread consumed (-1) and ibid.,
  -- results in -2.
  return (mkPosText (corPos start (-1)) (corPos end (-2)) t)


-- ------------------------------------------------------------
--
-- Character and Entity References (4.1)

referenceT :: XParser s XmlTree
referenceT =
  charRefT
  <|>
  entityRefT

charRefT :: XParser s XmlTree
charRefT = do
  start <- getPosition
  i <- XT.charRef
  end <- getPosition
  -- correct position: cf. char'
  return (mkPosCharRef i (corPos start (-1)) (corPos end (-2)))

entityRefT :: XParser s XmlTree
entityRefT = do
  start <- getPosition
  n <- XT.entityRefT
  end <- getPosition
  return $ ref n start end
  where
    -- correct position: cf. char'
    ref (XN.NTree (XEntityRef n) _) s e = mkTree (XN.mkEntityRef n) [(mkPositionNode (corPos s (-1)) (corPos e (-2)))]
    ref (XN.NTree (XCharRef i) _) s e = mkPosCharRef i (corPos s (-1)) (corPos e (-2))

-- ------------------------------------------------------------
--
-- Comments (2.5)

comment         :: XParser s XmlTree
comment
    = comment'' $ XT.checkString "<!--"

-- the leading <! is already parsed

comment'        :: XParser s XmlTree
comment'
    = comment'' (string "--" >> return ())

comment''       :: XParser s () -> XParser s XmlTree
comment'' op
    = ( do
        c <- between op (string ("-->")) (XT.allBut many "--")
        return (mkCmt' c)
      ) <?> "comment"

-- ------------------------------------------------------------
--
-- Processing Instructions

pI             :: XParser s XmlTree
pI = pI'' $ XT.checkString "<?"

-- the leading < is already parsed

pI'             :: XParser s XmlTree
pI' = pI'' (char '?' >> return ())

pI''             :: XParser s () -> XParser s XmlTree
pI'' op
    = between op (string "?>")
      ( do
        n <- pITarget
        p <- option "" (XT.sPace
                        >>
                        XT.allBut many "?>"
                       )
        return (mkPi' (mkName n) [mkAttr' (mkName a_value) [mkText' p]])
      ) <?> "processing instruction"
      where
      pITarget  :: XParser s String
      pITarget = ( do
                   n <- XT.name
                   if map toLower n == t_xml
                      then unexpected n
                      else return n
                 )

-- ------------------------------------------------------------
--
-- CDATA Sections (2.7)

cDSect          :: XParser s XmlTree
cDSect
    = cDSect'' $ XT.checkString "<![CDATA["

-- the leading <! is already parsed, no try neccessary

cDSect'         :: XParser s XmlTree
cDSect'
    = cDSect'' (string "[CDATA[" >> return ())

cDSect''        :: XParser s () -> XParser s XmlTree
cDSect'' op
    = do
      t <- between op (string "]]>") (XT.allBut many "]]>")
      return (mkCdata' t)
      <?> "CDATA section"

-- ------------------------------------------------------------
--
-- Document (2.1) and Prolog (2.8)

document        :: XParser s XmlTree
document
    = do
      pos <- getPosition
      dl <- document'
      return (mkRoot' [ mkAttr' (mkName a_source) [mkText' (sourceName pos)]
                      , mkAttr' (mkName a_status) [mkText' (show c_ok)]
                      ] dl
             )

document'       :: XParser s XmlTrees
document'
    = do
      pl <- XP.prolog
      el <- element
      ml <- many XP.misc
      eof
      return (pl ++ [el] ++ ml)

{-
prolog          :: XParser s XmlTrees
prolog
    = do
      xml     <- option [] xMLDecl'
      misc1   <- many misc
      dtdPart <- option [] doctypedecl
      misc2   <- many misc
      return (xml ++ misc1 ++ dtdPart ++ misc2)

xMLDecl         :: XParser s XmlTrees
xMLDecl
    = between (try $ string "<?xml") (string "?>")
      ( do
        vi <- versionInfo
        ed <- option [] encodingDecl
        sd <- option [] sDDecl
        XT.skipS0
        return (vi ++ ed ++ sd)
      )
      <?> "xml declaration"

xMLDecl'        :: XParser s XmlTrees
xMLDecl'
    = do
      al <- xMLDecl
      return [mkPi' (mkName t_xml) al]

xMLDecl''       :: XParser s XmlTree
xMLDecl''
    = do
      al     <- option [] (try xMLDecl)
      return (mkRoot' al [])

versionInfo     :: XParser s XmlTrees
versionInfo
    = ( do
        try ( XT.skipS
              >>
              XT.keyword a_version
              >>
              return ()
            )
        XT.eq
        vi <- XT.quoted XT.versionNum
        return [mkAttr' (mkName a_version) [mkText' vi]]
      )
      <?> "version info (with quoted version number)"

misc            :: XParser s XmlTree
misc
    = comment
      <|>
      pI
      <|>
      ( ( do
          ws <- XT.sPace
          return (mkText' ws)
        ) <?> ""
      )

-- ------------------------------------------------------------
--
-- Document Type definition (2.8)

doctypedecl     :: XParser s XmlTrees
doctypedecl
    = between (try $ string "<!DOCTYPE") (char '>')
      ( do
        XT.skipS
        n <- XT.name
        exId <- option [] ( try ( do
                                  XT.skipS
                                  externalID
                                )
                          )
        XT.skipS0
        markup <- option []
                  ( do
                    m <- between (char '[' ) (char ']') markupOrDeclSep
                    XT.skipS0
                    return m
                  )
        return [mkDTDElem' DOCTYPE ((a_name, n) : exId) markup]
      )

markupOrDeclSep :: XParser s XmlTrees
markupOrDeclSep
    = ( do
        ll <- many ( markupdecl
                     <|>
                     declSep
                     <|>
                     XT.mkList conditionalSect
                   )
        return (concat ll)
      )

declSep         :: XParser s XmlTrees
declSep
    = XT.mkList XT.peReferenceT
      <|>
      ( do
        XT.skipS
        return []
      )

markupdecl      :: XParser s XmlTrees
markupdecl
    = XT.mkList
      ( pI
        <|>
        comment
        <|>
        XD.dtdDeclTokenizer
      )

-- ------------------------------------------------------------
--
-- Standalone Document Declaration (2.9)

sDDecl          :: XParser s XmlTrees
sDDecl
    = do
      try ( XT.skipS
            >>
            XT.keyword a_standalone
            >>
            return ()
          )
      XT.eq
      sd <- XT.quoted (XT.keywords [v_yes, v_no])
      return [mkAttr' (mkName a_standalone) [mkText' sd]]
-}

-- ------------------------------------------------------------
--
-- element, tags and content (3, 3.1)

element         :: XParser s XmlTree
element
    = char '<'
      >>
      element'

element' :: XParser s XmlTree
element'
  = ( do
        start <- getPosition
        e@(n, al) <- elementStart
        rwnf e `seq` elementRest (n, al, start)           -- evaluate name and attribute list before parsing contents
      ) <?> "element"


elementStart            :: XParser s (QName, XmlTrees)
elementStart
    = do
      n  <- XT.name
      al <- attrList
      XT.skipS0
      return (mkName n, al)
      where
      attrList
          = option [] ( do
                        XT.skipS
                        attrList'
                      )
      attrList'
          = option [] ( do
                        a1 <- attribute
                        al <- attrList
                        let n = fromJust . getAttrName $ a1
                        if n `elem` map (fromJust . getAttrName) al
                          then unexpected
                               ( "attribute name " ++
                                 show (qualifiedName n) ++
                                 " occurs twice in attribute list"
                               )
                          else return (a1 : al)
                      )

elementRest     :: (QName, XmlTrees, SourcePos) -> XParser s XmlTree
elementRest (n, al, start)
    = ( do
        XT.checkString "/>"
        end <- getPosition
        return $ mkPosElement n (corPos start (-2)) (corPos end (-2)) al []
      )
      <|>
      ( do
        XT.gt
        c <- content
        eTag n
        end <- getPosition
        -- correct position: start: -2: the opening < was already
        -- consumed (-1) and SourcePos does not now column 0 (-1),
        -- results in -2. end: -2: the closing > was already consumed
        -- (-1) and ibid.
        return $ mkPosElement n (corPos start (-2)) (corPos end (-2)) al c
      )
      <?> "proper attribute list followed by \"/>\" or \">\""

eTag            :: QName -> XParser s ()
eTag n'
    = do
      XT.checkString "</" <?> ""
      n <- XT.name
      XT.skipS0
      XT.gt
      if n == qualifiedName n'
         then return ()
         else unexpected ("illegal end tag </" ++ n ++ "> found, </" ++ qualifiedName n' ++ "> expected")


-- Export this!!!
attribute       :: XParser s XmlTree
attribute
    = do
      n <- XT.name
      XT.eq
      v <- XT.attrValueT
      return $ mkAttr' (mkName n) v

{- this parser corresponds to the XML spec but it's inefficent because of more than 1 char lookahead

content         :: XParser s XmlTrees
content
    = do
      c1 <- charData
      cl <- many
            ( do
              l <- ( element
                     <|>
                     cDSect
                     <|>
                     pI
                     <|>
                     comment
                   )
              c <- charData
              return (l : c)
            )
      return (c1 ++ concat cl)
-}

-- this simpler content parser does not need more than a single lookahead
-- so no try parsers (inefficient) are neccessary

content         :: XParser s XmlTrees
content
    = many
      ( ( do            -- parse markup but no closing tags
          try ( XT.lt
                >>
                notFollowedBy (char '/')
                >>
                return ()
              )
          markup
        )
        <|>
        charData'
        <|>
        referenceT
      )
    where
    markup
        = element'
          <|>
          pI'
          <|>
          ( char '!'
            >>
            ( comment'
              <|>
              cDSect'
            )
          )

contentWithTextDecl     :: XParser s XmlTrees
contentWithTextDecl
    = option [] XP.textDecl
      >>
      content

{-
-- ------------------------------------------------------------
--
-- Conditional Sections (3.4)
--
-- conditional sections are parsed in two steps,
-- first the whole content is detected,
-- and then, after PE substitution include sections are parsed again

conditionalSect         :: XParser s XmlTree
conditionalSect
    = do
      XT.checkString "<!["
      cs <- many XD.dtdToken
      _ <- char '['
      sect <- condSectCont
      return (mkDTDElem' CONDSECT [(a_value, sect)] cs)
    where

    condSectCont        :: XParser s String
    condSectCont
        = ( XT.checkString "]]>"
            >>
            return ""
          )
          <|>
          ( do
            XT.checkString "<!["
            cs1 <- condSectCont
            cs2 <- condSectCont
            return ("<![" ++ cs1 ++ "]]>" ++ cs2)
          )
          <|>
          ( do
            c  <- xmlChar
            cs <- condSectCont
            return (c : cs)
          )

-- ------------------------------------------------------------
--
-- External Entities (4.2.2)

externalID      :: XParser s Attributes
externalID
    = ( do
        _ <- XT.keyword k_system
        XT.skipS
        lit <- XT.systemLiteral
        return [(k_system, lit)]
      )
      <|>
      ( do
        _ <- XT.keyword k_public
        XT.skipS
        pl <- XT.pubidLiteral
        XT.skipS
        sl <- XT.systemLiteral
        return [ (k_system, sl)
               , (k_public, pl) ]
      )
      <?> "SYSTEM or PUBLIC declaration"

-- ------------------------------------------------------------
--
-- Text Declaration (4.3.1)

textDecl        :: XParser s XmlTrees
textDecl
    = between (try $ string "<?xml") (string "?>")
      ( do
        vi <- option [] versionInfo
        ed <- encodingDecl
        XT.skipS0
        return (vi ++ ed)
      )
      <?> "text declaration"


textDecl''      :: XParser s XmlTree
textDecl''
    = do
      al    <- option [] (try textDecl)
      return (mkRoot' al [])

-- ------------------------------------------------------------
--
-- Encoding Declaration (4.3.3)

encodingDecl    :: XParser s XmlTrees
encodingDecl
    = do
      try ( XT.skipS
            >>
            XT.keyword a_encoding
            >>
            return ()
          )
      XT.eq
      ed <- XT.quoted XT.encName
      return [mkAttr' (mkName a_encoding) [mkText' ed]]
-}
-- ------------------------------------------------------------
--
-- the main entry points:
--      parsing the content of a text node
--      or parsing the text children from a tag node

-- |
-- the inverse function to 'xshow', (for XML content).
--
-- the string parameter is parsed with the XML content parser.
-- result is the list of trees or in case of an error a single element list with the
-- error message as node. No entity or character subtitution is done here,
-- but the XML parser can do this for the predefined XML or the char references for performance reasons
--
-- see also: 'parseXmlContent'

xread                   :: String -> XmlTrees
xread                   = xread' content         -- take the content parser for parsing the string

xreadDoc                :: String -> XmlTrees
xreadDoc                = xread' document'       -- take the document' parser for parsing the string

xread'                   :: XParser () XmlTrees -> String -> XmlTrees
xread' content' str
    = parseXmlFromString parser (withNormNewline ()) loc str
    where
    loc = "string: " ++ show (if length str > 40 then take 40 str ++ "..." else str)
    parser = do
             res <- content'
             eof                        -- test on everything consumed
             return res

-- |
-- the filter version of 'xread'

parseXmlContent         :: XmlTree -> XmlTrees
parseXmlContent
    = xread . xshow . (:[])

-- |
-- a more general version of 'parseXmlContent'.
-- The parser to be used and the context are extra parameter

parseXmlText            :: XParser s XmlTrees -> XPState s -> String -> XmlTree -> XmlTrees
parseXmlText p s0 loc   = parseXmlFromString p s0 loc . xshow . (:[])

parseXmlDocument        :: String -> String -> XmlTrees
parseXmlDocument        = parseXmlFromString document' (withNormNewline ())

parseXmlDocumentWithUserState :: XPState s -> String -> String -> XmlTrees
parseXmlDocumentWithUserState s0 = parseXmlFromString document' (withNormNewline s0)

parseXmlFromString      :: XParser s XmlTrees -> XPState s -> String -> String -> XmlTrees
parseXmlFromString parser s0 loc
    = either ((:[]) . mkError' c_err . (++ "\n") . show) id
      . runParser parser s0 loc

-- ------------------------------------------------------------
--

-- |
-- general parser for parsing arbitray parts of a XML document

parseXmlPart    :: SimpleXParser XmlTrees -> String -> String -> XmlTree -> XmlTrees
parseXmlPart parser expected context t
    = parseXmlText
      ( do
        res <- parser
        eof <?> expected
        return res
      ) (withoutNormNewline ()) context
      $ t

-- ------------------------------------------------------------

-- |
-- Parser for general entites

parseXmlEntityValueAsContent      :: String -> XmlTree -> XmlTrees
parseXmlEntityValueAsContent
    = parseXmlPart content "general entity value"

