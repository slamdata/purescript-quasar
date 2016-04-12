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
  ( Config(..)
  , Host
  , fromJSON
  , toJSON
  ) where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array as Arr
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Path.Pathy (AbsFile, AbsDir, Sandboxed, printPath)
import Data.String as Str
import Data.String.Regex as Rgx
import Data.StrMap as SM
import Data.Tuple (Tuple(..), uncurry)
import Data.URI (runParseAbsoluteURI)
import Data.URI.Types as URI

import Global (encodeURIComponent, decodeURIComponent)

type Config =
  { hosts ∷ Array Host
  , path ∷ Maybe (Either (AbsDir Sandboxed) (AbsFile Sandboxed))
  , user ∷ Maybe String
  , password ∷ Maybe String
  , props ∷ SM.StrMap (Maybe String)
  }

type Host = Tuple URI.Host (Maybe URI.Port)

fromJSON ∷ Json → Either String Config
fromJSON = decodeJson >=> \obj → do
  connStr ← obj .? "mongodb" >>= decodeJson >>= (_ .? "connectionUri")
  uri ← lmap show $ runParseAbsoluteURI connStr
  pure $ fromURI uri

toJSON ∷ Config → Json
toJSON config =
  "mongodb" := ("connectionUri" := unit ~> jsonEmptyObject) ~> jsonEmptyObject

fromURI ∷ URI.AbsoluteURI → Config
fromURI uri =
  { hosts: hostsFromURI uri
  , path: pathFromURI uri
  , user: userFromURI uri
  , password: passwordFromURI uri
  , props: propsFromURI uri
  }

-- TODO: encode/decode space as + in quasar URLs

hostsFromURI ∷ URI.AbsoluteURI → Array Host
hostsFromURI (URI.AbsoluteURI _ (URI.HierarchicalPart (Just (URI.Authority _ hs)) _) _) = hs
hostsFromURI _ = []

pathFromURI ∷ URI.AbsoluteURI → Maybe (Either (AbsDir Sandboxed) (AbsFile Sandboxed))
pathFromURI (URI.AbsoluteURI _ (URI.HierarchicalPart _ p) _) = either Right Left <$> p

userFromURI ∷ URI.AbsoluteURI → Maybe String
userFromURI (URI.AbsoluteURI _ (URI.HierarchicalPart (Just (URI.Authority (Just ui) _)) _) _) =
  map (\ix → decodeURIComponent $ Str.take ix ui) $ Str.indexOf ":" ui
userFromURI _ = Nothing

passwordFromURI ∷ URI.AbsoluteURI → Maybe String
passwordFromURI (URI.AbsoluteURI _ (URI.HierarchicalPart (Just (URI.Authority (Just ui) _)) _) _) =
  map (\ix → decodeURIComponent $ Str.drop (ix + 1) ui) $ Str.indexOf ":" ui
passwordFromURI _ = Nothing

propsFromURI ∷ URI.AbsoluteURI → SM.StrMap (Maybe String)
propsFromURI (URI.AbsoluteURI _ _ (Just (URI.Query qs))) = qs
propsFromURI _ = SM.empty

mkURI ∷ Config → URI.AbsoluteURI
mkURI { hosts, path, user, password, props } =
  URI.AbsoluteURI
    (Just (URI.URIScheme "mongodb"))
    (URI.HierarchicalPart Nothing Nothing)
    Nothing
