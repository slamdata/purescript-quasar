{-
Copyright 2017 SlamData, Inc.

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

module Quasar.Mount.MarkLogic
  ( Config
  , Format(..)
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Codec (decode, encode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Pathy (AbsPath)
import Quasar.URI as URI
import URI.Scheme as Scheme

type Config =
  { host ∷ URI.QURIHost
  , path ∷ Maybe AbsPath
  , credentials ∷ Maybe URI.UserPassInfo
  , format ∷ Format
  }

data Format
  = JSON
  | XML

derive instance eqFormat ∷ Eq Format
derive instance ordFormat ∷ Ord Format

instance showFormat ∷ Show Format where
  show = case _ of
    JSON → "JSON"
    XML → "XML"

toJSON ∷ Config → Json
toJSON config =
  let uri = encode URI.qAbsoluteURI (toURI config)
  in "marklogic" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< decode URI.qAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "marklogic")
  <=< decodeJson

toURI ∷ Config → URI.QAbsoluteURI
toURI { host, path, credentials, format } =
  URI.AbsoluteURI
    uriScheme
    (URI.HierarchicalPartAuth (URI.Authority credentials host) path)
    (Just (URI.QueryPairs [ (Tuple "format" (Just formatStr)) ]))
  where
  formatStr ∷ String
  formatStr = case format of
    JSON → "json"
    XML  → "xml"

fromURI ∷ URI.QAbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI _ (URI.HierarchicalPartNoAuth _) _) = do
  Left "Expected 'auth' part in URI"
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPartAuth (URI.Authority credentials host) path) query) = do
  unless (scheme == uriScheme) $ Left "Expected 'xcc' URL scheme"
  let
    props = maybe SM.empty (\(URI.QueryPairs qs) → SM.fromFoldable qs) query
  format ← case join $ SM.lookup "format" props of
    Nothing → pure XML
    Just "xml" → pure XML
    Just "json" → pure JSON
    Just f → Left $ "Unexpected format: " <> f
  pure { host, path, credentials, format}

uriScheme ∷ URI.Scheme
uriScheme = Scheme.unsafeFromString "xcc"
