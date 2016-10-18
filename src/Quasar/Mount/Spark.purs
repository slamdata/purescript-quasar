{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Quasar.Mount.Spark
  ( Config
  , toJSON
  , fromJSON
  , toString
  , fromString
  , module Exports
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith, split)
import Data.URI as URI
import Data.URI.Path (printPath, parseURIPathAbs)

import Quasar.Mount.Common (Host, extractHost)
import Quasar.Mount.Common (Host) as Exports
import Quasar.Types (AnyPath)

import Text.Parsing.StringParser (runParser)

type Config =
  { sparkHost ∷ Host
  , hdfsHost ∷ Host
  , path ∷ Maybe AnyPath
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = toString config
  in "spark" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromString
  <=< (_ .? "connectionUri")
  <=< (_ .? "spark")
  <=< decodeJson

toString ∷ Config → String
toString { sparkHost, hdfsHost, path } =
  joinWith "|"
    [ URI.printAbsoluteURI (mkURI sparkURIScheme sparkHost)
    , URI.printAbsoluteURI (mkURI hdfsURIScheme hdfsHost)
    , maybe "/" printPath path
    ]
  where
  mkURI ∷ URI.URIScheme → Host → URI.AbsoluteURI
  mkURI scheme host =
    URI.AbsoluteURI
      (Just scheme)
      (URI.HierarchicalPart (Just (URI.Authority Nothing (pure host))) Nothing)
      Nothing

fromString ∷ String → Either String Config
fromString str = case split "|" str of
  [ spark, hdfs, root ] → do
    sparkHost ← extractHost' sparkURIScheme spark
    hdfsHost ← extractHost' hdfsURIScheme hdfs
    path ← bimap show Just (runParser parseURIPathAbs root)
    pure
      { sparkHost
      , hdfsHost
      , path
      }
  _ →
    Left "Expected 'spark' connectionUri format"
  where
  extractHost' ∷ URI.URIScheme → String → Either String Host
  extractHost' scheme@(URI.URIScheme name) uri = do
    URI.AbsoluteURI scheme' (URI.HierarchicalPart auth _) _ ←
      lmap show $ URI.runParseAbsoluteURI uri
    unless (scheme' == Just scheme) $ Left $ "Expected '" <> name <> "' URL scheme"
    extractHost auth

sparkURIScheme ∷ URI.URIScheme
sparkURIScheme = URI.URIScheme "spark"

hdfsURIScheme ∷ URI.URIScheme
hdfsURIScheme = URI.URIScheme "hdfs"
