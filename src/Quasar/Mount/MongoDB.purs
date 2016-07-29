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

module Quasar.Mount.MongoDB
  ( Config
  , Host
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  ) where

import Prelude

import Data.Array as Arr
import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..), oneOf)
import Data.String as Str
import Data.StrMap as SM
import Data.Tuple (Tuple)
import Data.URI as URI

import Quasar.Types (AnyPath)

type Config =
  { hosts ∷ NonEmpty Array Host
  , path ∷ Maybe AnyPath
  , user ∷ Maybe String
  , password ∷ Maybe String
  , props ∷ SM.StrMap (Maybe String)
  }

type Host = Tuple URI.Host (Maybe URI.Port)

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
toURI { hosts, path, user, password, props } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart (Just (URI.Authority userInfo (oneOf hosts))) path)
    (Just (URI.Query props))
  where
  userInfo = do
    u ← user
    p ← password
    pure (u <> ":" <> p)

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth path) query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'mongodb' URL scheme"
  hosts ← extractHosts auth
  let creds = extractCredentials auth
  let props = maybe SM.empty (\(URI.Query qs) → qs) query
  pure { hosts, path, user: creds.user, password: creds.password, props }

uriScheme ∷ URI.URIScheme
uriScheme = URI.URIScheme "mongodb"

extractHosts ∷ Maybe URI.Authority → Either String (NonEmpty Array Host)
extractHosts = maybe err Right <<< (toNonEmpty <=< map getHosts)
  where
  getHosts (URI.Authority _ hs) = hs
  toNonEmpty hs = NonEmpty <$> Arr.head hs <*> Arr.tail hs
  err = Left "Host list must not be empty"

extractCredentials ∷ Maybe URI.Authority → { user ∷ Maybe String, password ∷ Maybe String }
extractCredentials auth =
  case auth >>= (\(URI.Authority userInfo _) → userInfo) of
    Nothing → { user: Nothing, password: Nothing }
    Just userInfo →
      case Str.indexOf ":" userInfo of
        Nothing → { user: Just userInfo, password: Nothing }
        Just ix →
          { user: Just (Str.take ix userInfo)
          , password: Just (Str.drop (ix + 1) userInfo)
          }
