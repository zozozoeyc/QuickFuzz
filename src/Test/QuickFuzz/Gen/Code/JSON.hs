{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.C where

import Data.Default

import Data.Aeson
import Test.QuickCheck
import Control.Monad
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString as BS

-- import Data.ByteString.Lazy.Char8 as L8
-- import Data.ByteString.Lazy as L
-- import Data.Text (unpack)
-- import Data.Text.Encoding (decodeUtf8)
--
-- import Language.ECMAScript3.PrettyPrint
-- import Language.ECMAScript3.Syntax
-- import Language.ECMAScript3.Parser

instance NFData Position where
  rnf x = ()

--data CCode = CCode (CTranslationUnit NodeInfo)
--unCCode (CCode x) = x

devArbitrary ''JSON
devNFData ''JSON
devMutation ''JSON

decodeJSON x = either undefined id $ Data.Aeson.decode

jsonInfo :: FormatInfo JSON NoActions
jsonInfo = def
    { -- encode = L8.pack . concat . (map show) . (map pretty)\
      encode = Data.Aeson.Encode.encode
    , decode = decodeJSON
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "json"
    }
