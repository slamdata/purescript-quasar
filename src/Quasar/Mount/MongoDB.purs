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
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Pathy as P
import Data.StrMap as SM
import Quasar.Data.URI as URI
import Quasar.Types (AnyPath)
import Text.Parsing.Parser (runParser)

newtype Auth = Auth { path ∷ AnyPath, credentials ∷ URI.UserPassInfo }

derive instance newtypeAuth ∷ Newtype Auth _
derive instance eqAuth ∷ Eq Auth
derive instance ordAuth ∷ Ord Auth

instance showAuth ∷ Show Auth where
  show (Auth { path, credentials }) =
    "(Auth { path: " <> show path <> ", credentials: " <> show credentials <> " })"

type Config =
  { hosts ∷ URI.QURIHost
  -- { hosts ∷ NonEmpty Array Host
  , auth ∷ Maybe Auth
  , props ∷ SM.StrMap (Maybe String)
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.qAbsoluteURI.print (toURI config)
  in "mongodb" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< flip runParser URI.qAbsoluteURI.parser
  <=< (_ .? "connectionUri")
  <=< (_ .? "mongodb")
  <=< decodeJson

toURI ∷ Config → URI.QAbsoluteURI
toURI { hosts, auth, props } =
  URI.AbsoluteURI
    uriScheme
    (URI.HierarchicalPartAuth
      (URI.Authority
        (_.credentials <<< unwrap <$> auth)
        hosts)
      (map (_.path <<< unwrap) auth <|> Just (Left P.rootDir)))
    (if null props
      then Nothing
      else Just (URI.QueryPairs (SM.toUnfoldable props)))


fromURI ∷ URI.QAbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI _ (URI.HierarchicalPartNoAuth _) _) = do
  Left "Expected 'auth' part in URI"
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPartAuth (URI.Authority credentials hosts) path) query) = do
  unless (scheme == uriScheme) $ Left "Expected 'mongodb' URL scheme"
  auth' ← case credentials, path of
    Just c, Just p → pure $ Just (Auth { path: p, credentials: c })
    Nothing, Nothing → pure $ Nothing
    Just _, Nothing → Left "User credentials were specified, but no auth database"
    Nothing, Just p
      | p /= Left P.rootDir → Left "An auth database was specified, but no user credentials"
      | otherwise → pure $ Nothing
  let props = maybe SM.empty (\(URI.QueryPairs qs) → SM.fromFoldable qs) query
  pure { hosts, auth: auth', props }

uriScheme ∷ URI.Scheme
uriScheme = URI.unsafeSchemaFromString "mongodb"
