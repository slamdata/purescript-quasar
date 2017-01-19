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

module Quasar.Mount.MarkLogic
  ( Config
  , Format(..)
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  , module Exports
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.URI as URI

import Quasar.Mount.Common (Host, credentials, extractCredentials, extractHost)
import Quasar.Mount.Common (Host) as Exports
import Quasar.Types (AnyPath)

type Config =
  { host ∷ Host
  , path ∷ Maybe AnyPath
  , user ∷ Maybe String
  , password ∷ Maybe String
  , format ∷ Format
  }

data Format
  = JSON
  | XML

derive instance eqFormat ∷ Eq Format
derive instance ordFormat ∷ Ord Format

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.printAbsoluteURI (toURI config)
  in "marklogic" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< URI.runParseAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "marklogic")
  <=< decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { host, path, user, password, format } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart (Just (URI.Authority (credentials user password) (pure host))) path)
    (Just (URI.Query props))
  where
  props ∷ L.List (Tuple String (Maybe String))
  props = L.singleton (Tuple "format" (Just formatStr))

  formatStr ∷ String
  formatStr = case format of
    JSON → "json"
    XML  → "xml"

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth path) query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'xcc' URL scheme"
  host ← extractHost auth
  let
    creds = extractCredentials auth
    props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query
  format ← case join $ SM.lookup "format" props of
    Nothing → pure XML
    Just "xml" → pure XML
    Just "json" → pure JSON
    Just f → Left $ "Unexpected format: " <> f
  pure
    { host
    , path
    , user: creds.user
    , password: creds.password
    , format
    }

uriScheme ∷ URI.URIScheme
uriScheme = URI.URIScheme "xcc"
