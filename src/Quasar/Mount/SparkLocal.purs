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

module Quasar.Mount.SparkLocal
  ( Config
  , toJSON
  , fromJSON
  , toString
  , fromString
  , module Exports
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.URI as URI
import Data.URI.Path (printPath, parseURIPathAbs)

import Quasar.Mount.Common (Host) as Exports
import Quasar.Types (AnyPath)

import Text.Parsing.StringParser (runParser)

type Config =
  { path ∷ Maybe AnyPath }

toJSON ∷ Config → Json
toJSON config =
  let uri = toString config
  in "spark-local" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromString
  <=< (_ .? "connectionUri")
  <=< (_ .? "spark-local")
  <=< decodeJson

toString ∷ Config → String
toString { path } =
  joinWith "|"
    [ maybe "/" printPath path ]

fromString ∷ String → Either String Config
fromString str = case split (Pattern "|") str of
  [ root ] → do
    path ← bimap show Just (runParser parseURIPathAbs root)
    pure
      { path }
  _ →
    Left "Expected 'spark-local' connectionUri format"

sparkURIScheme ∷ URI.URIScheme
sparkURIScheme = URI.URIScheme "spark"

localURIScheme ∷ URI.URIScheme
localURIScheme = URI.URIScheme "local"
