-- ------------------------------------------------------------

{- |
   Module     : Text.XML.HXT.Arrow.ProcessDocument
   Copyright  : Copyright (C) 2011 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Compound arrows for reading, parsing, validating and writing XML documents

   All arrows use IO and a global state for options, errorhandling, ...
-}

-- ------------------------------------------------------------

module HTCF.PosParser.ProcessDocument
    ( parseXmlDocument
    )
where

import           Control.Arrow
import           Control.Arrow.ArrowIf
import           Control.Arrow.ArrowList
import           Control.Arrow.ArrowTree
import           Control.Arrow.ListArrow                      (fromLA)
import           Control.Arrow.NTreeEdit

import           Text.XML.HXT.DOM.Interface

import           Text.XML.HXT.Arrow.XmlArrow
import           Text.XML.HXT.Arrow.XmlState
import           Text.XML.HXT.Arrow.XmlState.TypeDefs

import           Text.XML.HXT.Arrow.Edit                      (substAllXHTMLEntityRefs,
                                                               transfAllCharRef)

import           Text.XML.HXT.Arrow.GeneralEntitySubstitution (processGeneralEntities)

import           Text.XML.HXT.Arrow.DTDProcessing             (processDTD)

import           Text.XML.HXT.Arrow.DocumentInput             (getXmlContents)

import           Text.XML.HXT.Arrow.Namespace                 (propagateNamespaces, validateNamespaces)
import           Text.XML.HXT.DTDValidation.Validation        (generalEntitiesDefined,
                                                               getDTDSubset,
                                                               transform,
                                                               validate)

import           Text.XML.HXT.Arrow.ProcessDocument           (validateDocument)

import           HTCF.PosParser.ParserInterface               (parseXmlDoc)

-- ------------------------------------------------------------

{- |
XML parser

Input tree must be a root tree with a text tree as child containing the document to be parsed.
The parser generates from the input string a tree of a wellformed XML document,
processes the DTD (parameter substitution, conditional DTD parts, ...) and
substitutes all general entity references. Next step is character reference substitution.
Last step is the document validation.
Validation can be controlled by an extra parameter.

Example:

> parseXmlDocument True    -- parse and validate document
>
> parseXmlDocument False   -- only parse document, don't validate

This parser is useful for applications processing correct XML documents.
-}

parseXmlDocument        :: Bool -> Bool -> Bool -> Bool -> IOStateArrow s XmlTree XmlTree
parseXmlDocument validateD substDTD substHTML validateRX
    = ( replaceChildren ( ( getAttrValue a_source
                            &&&
                            xshow getChildren
                          )
                          >>>
                          parseXmlDoc
                          >>>
                          filterErrorMsg
                        )
        >>>
        setDocumentStatusFromSystemState "parse XML document"
        >>>
        ( ifA (fromLA getDTDSubset)
          ( processDTDandEntities
            >>>
            ( if validate'                      -- validation only possible if there is a DTD
              then validateDocument
              else this
            )
          )
          ( if validate'                        -- validation only consists of checking
                                                -- for undefined entity refs
                                                -- predefined XML entity refs are substituted
                                                -- in the XML parser into char refs
                                                -- so there is no need for an entity substitution
            then traceMsg 2 "checkUndefinedEntityRefs: looking for undefined entity refs"
                 >>>
                 perform checkUndefinedEntityRefs
                 >>>
                 traceMsg 2 "checkUndefinedEntityRefs: looking for undefined entity refs done"
                 >>>
                 setDocumentStatusFromSystemState "decoding document"
            else this
          )
        )
      )
      `when` documentStatusOk
    where
      validate'
          = validateD && not validateRX

      processDTDandEntities
          = ( if validateD || substDTD
              then processDTD
              else this
            )
            >>>
            ( if substDTD
              then ( processGeneralEntities             -- DTD contains general entity definitions
                     `when`
                     fromLA generalEntitiesDefined
                   )
              else if substHTML
                   then substAllXHTMLEntityRefs
                   else this
            )
            >>>
            transfAllCharRef


-- | not exported in hxt
checkUndefinedEntityRefs        :: IOStateArrow s XmlTree XmlTree
checkUndefinedEntityRefs
    = deep isEntityRef
      >>>
      getEntityRef
      >>>
      arr (\ en -> "general entity reference \"&" ++ en ++ ";\" is undefined")
      >>>
      mkError c_err
      >>>
      filterErrorMsg

