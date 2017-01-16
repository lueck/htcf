{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.Text.XML.TCF.Arrow.ArrowXml
import {-@ HTF_TESTS @-} Test.Text.XML.TCF.Parser.ConfigParser

main = htfMain htf_importedTests
