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

module Quasar.Mount.MongoDB
  ( Config
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  , module Exports
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..), oneOf)
import Data.StrMap as SM
import Data.URI as URI

import Quasar.Mount.Common (Host, Credentials, combineCredentials, extractCredentials)
import Quasar.Mount.Common (Host, Credentials(..)) as Exports
import Quasar.Types (AnyPath)

type Config =
  { hosts ∷ NonEmpty Array Host
  , path ∷ Maybe AnyPath
  , credentials ∷ Maybe Credentials
  , props ∷ SM.StrMap (Maybe String)
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.printAbsoluteURI (toURI config)
  in "mongodb" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< URI.runParseAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "mongodb")
  <=< decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { hosts, path, credentials, props } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart (Just (URI.Authority (combineCredentials <$> credentials) (oneOf hosts))) path)
    (Just (URI.Query (SM.toUnfoldable props)))

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth path) query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'mongodb' URL scheme"
  hosts ← extractHosts auth
  let credentials = extractCredentials auth
  let props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query
  pure { hosts, path, credentials, props }

uriScheme ∷ URI.URIScheme
uriScheme = URI.URIScheme "mongodb"

extractHosts ∷ Maybe URI.Authority → Either String (NonEmpty Array Host)
extractHosts = maybe err Right <<< (toNonEmpty <=< map getHosts)
  where
  getHosts (URI.Authority _ hs) = hs
  toNonEmpty hs = NonEmpty <$> Arr.head hs <*> Arr.tail hs
  err = Left "Host list must not be empty"
