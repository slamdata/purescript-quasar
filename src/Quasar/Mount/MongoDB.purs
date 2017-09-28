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
  , Auth(..)
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
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..), oneOf)
import Data.StrMap as SM
import Data.URI as URI
import Data.URI.AbsoluteURI as AbsoluteURI
import Quasar.Mount.Common (Host, Credentials(..)) as Exports
import Quasar.Mount.Common (Host, Credentials, combineCredentials, extractCredentials)
import Quasar.Types (AnyPath)

newtype Auth = Auth { path ∷ AnyPath, credentials ∷ Credentials }

derive instance newtypeAuth ∷ Newtype Auth _
derive instance eqAuth ∷ Eq Auth
derive instance ordAuth ∷ Ord Auth

instance showAuth ∷ Show Auth where
  show (Auth { path, credentials }) =
    "(Auth { path: " <> show path <> ", credentials: " <> show credentials <> " })"

type Config =
  { hosts ∷ NonEmpty Array Host
  , auth ∷ Maybe Auth
  , props ∷ SM.StrMap (Maybe String)
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = AbsoluteURI.print (toURI config)
  in "mongodb" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< AbsoluteURI.parse
  <=< (_ .? "connectionUri")
  <=< (_ .? "mongodb")
  <=< decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { hosts, auth, props } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart
      (Just
        (URI.Authority
          (combineCredentials <<< _.credentials <<< unwrap <$> auth)
          (oneOf hosts)))
      (_.path <<< unwrap <$> auth))
    (Just (URI.Query (SM.toUnfoldable props)))

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth path) query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'mongodb' URL scheme"
  hosts ← extractHosts auth
  auth' ← case extractCredentials auth, path of
    Just credentials, Just p → pure $ Just (Auth { path: p, credentials })
    Nothing, Nothing → pure $ Nothing
    Just _, Nothing → Left "User credentials were specified, but no auth database"
    Nothing, Just _ → Left "An auth database was specified, but no user credentials"
  let props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query
  pure { hosts, auth: auth', props }

uriScheme ∷ URI.Scheme
uriScheme = URI.Scheme "mongodb"

extractHosts ∷ Maybe URI.Authority → Either String (NonEmpty Array Host)
extractHosts = maybe err Right <<< (toNonEmpty <=< map getHosts)
  where
  getHosts (URI.Authority _ hs) = hs
  toNonEmpty hs = NonEmpty <$> Arr.head hs <*> Arr.tail hs
  err = Left "Host list must not be empty"
