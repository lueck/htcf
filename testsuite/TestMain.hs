{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.HTCF.ArrowXml
import {-@ HTF_TESTS @-} Test.HTCF.ConfigParser
import {-@ HTF_TESTS @-} Test.HTCF.Tokenizer

main = htfMain htf_importedTests
