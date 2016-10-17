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

module Quasar.Mount.Couchbase
  ( Config
  , Host
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.URI as URI

type Config =
  { host ∷ Host
  , user ∷ Maybe String
  , password ∷ Maybe String
  }

type Host = Tuple URI.Host (Maybe URI.Port)

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.printAbsoluteURI (toURI config)
  in "couchbase" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< URI.runParseAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "couchbase")
  <=< decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { host, user, password } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart (Just (URI.Authority Nothing (pure host))) Nothing)
    (Just (URI.Query $ SM.fromFoldable props))
  where
  props :: Array (Tuple String (Maybe String))
  props = []
    <> maybe [] (\u -> [Tuple "username" (Just u)]) user
    <> maybe [] (\p -> [Tuple "password" (Just p)]) user

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth path) query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'couchbase' URL scheme"
  host ← extractHost auth
  let props = maybe SM.empty (\(URI.Query qs) → qs) query
  pure
    { host
    , user: join $ SM.lookup "username" props
    , password: join $ SM.lookup "password" props
    }

uriScheme ∷ URI.URIScheme
uriScheme = URI.URIScheme "couchbase"

extractHost ∷ Maybe URI.Authority → Either String Host
extractHost (Just (URI.Authority _ hs)) =
  case hs of
    [h] -> Right h
    [] -> Left "No host specified"
    _ -> Left "Multiple hosts specified"
extractHost _ = Left "No host specified"
