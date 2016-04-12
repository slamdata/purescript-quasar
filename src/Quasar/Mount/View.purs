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

module Quasar.Mount.View where

import Prelude

import Control.Bind ((>=>), (=<<))
import Control.Monad (unless)

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (~>), (:=))
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Either (Either(..))
import Data.String as Str
import Data.String.Regex as Rgx
import Data.StrMap as SM
import Data.List (List, (:))
import Data.List as List
import Data.Tuple (Tuple(..))

import Global (encodeURIComponent, decodeURIComponent)

import Quasar.Types (SQL, Vars)
import Data.URI (runParseAbsoluteURI) as URI
import Data.URI.Types (AbsoluteURI(..), Query(..), URIScheme(..)) as URI

type Config =
  { query ∷ SQL
  , vars ∷ Vars
  }

toJSON ∷ Config → Json
toJSON config
  = "view" := ("connectionUri" := toURI config ~> jsonEmptyObject)
  ~> jsonEmptyObject

toURI ∷ Config → String
toURI { query, vars }
  = "sql2://"
  <> (toQueryString
       $ Tuple "q" query
       : (lmap ("var." <> _) <$> SM.toList vars))
  where

  toQueryString ∷ List (Tuple String String) → String
  toQueryString
    = ("?" <> _)
    <<< Str.joinWith "&"
    <<< List.toUnfoldable
    <<< map (\(Tuple k v) → k <> "=" <> encodeURIComponent v)

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI mbScheme _ mbQuery) = do
  scheme ← maybe (Left "Could not parse URL scheme") Right mbScheme
  unless (scheme == URI.URIScheme "sql2") $ Left "Expected 'sql2' URL scheme"
  let queryMap = maybe SM.empty runQuery mbQuery
  query ←
    maybe (Left "Expected 'q' query variable") pure
      $ SM.lookup "q" queryMap
      >>= id
      >>> map decodeURIPath
      >>= Str.stripPrefix "("
      >>= Str.stripSuffix ")"
  let vars = SM.fold foldFn SM.empty $ SM.delete "q" queryMap
  pure { query, vars }

  where

  runQuery ∷ URI.Query → SM.StrMap (Maybe String)
  runQuery (URI.Query q) = q

  decodeURIPath :: String -> String
  decodeURIPath uri =
    decodeURIComponent $
    Rgx.replace (Rgx.regex "\\+" Rgx.noFlags{global=true}) " " uri

  foldFn ∷ SM.StrMap String → String → Maybe String → SM.StrMap String
  foldFn acc key mbVal = fromMaybe acc do
    k ← Str.stripPrefix "var." key
    val ← mbVal
    pure $ SM.insert k val acc

fromJSON ∷ Json → Either String Config
fromJSON = decodeJson >=> \obj → do
  connStr ← obj .? "view" >>= decodeJson >>= (_ .? "connectionUri")
  fromURI =<< lmap show (URI.runParseAbsoluteURI connStr)
