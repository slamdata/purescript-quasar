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

module Quasar.Mount.Couchbase
  ( Config
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
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (un)
import Data.Number as Num
import Data.StrMap as SM
import Data.String.NonEmpty as NES
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Pathy (Name(..), (</>))
import Pathy as P
import Quasar.URI as URI
import URI.Scheme as Scheme

type Config =
  { host ∷ URI.QURIHost'
  , bucketName ∷ String
  , password ∷ String
  , docTypeKey ∷ String
  , queryTimeout ∷ Maybe Seconds
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = encode URI.qAbsoluteURI (toURI config)
  in "couchbase" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< decode URI.qAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "couchbase")
  <=< decodeJson

toURI ∷ Config → URI.QAbsoluteURI
toURI { host, bucketName, password, docTypeKey, queryTimeout } =
  URI.AbsoluteURI
    uriScheme
    hierarchicalPart
    (Just $ URI.QueryPairs props)
  where
  hierarchicalPart ∷ URI.QHierarchicalPart
  hierarchicalPart =
    URI.HierarchicalPartAuth
      (URI.Authority Nothing $ Just host)
      case NES.fromString bucketName of
        Nothing → Just $ Left P.rootDir
        Just n → Just $ Right $ P.rootDir </> P.file' (Name n)

  props ∷ Array (Tuple String (Maybe String))
  props =
    [ Tuple "password" (Just password)
    , Tuple "docTypeKey" (Just docTypeKey)
    ] <> maybe [] (pure <<< Tuple "queryTimeoutSeconds" <<< Just <<< show <<< un Seconds) queryTimeout

fromURI ∷ URI.QAbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPartNoAuth path) query) =
  Left "Expected 'auth' part in URI"
fromURI (URI.AbsoluteURI _ (URI.HierarchicalPartAuth (URI.Authority _ Nothing) _) _) = do
  Left "Expected 'host' part to be present in URL"
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPartAuth (URI.Authority _ (Just host)) path) query) = do
  unless (scheme == uriScheme) $ Left "Expected 'couchbase' URL scheme"
  bucketName ← case path of
    Nothing → Left "Path is missing from URL"
    Just (Left p)
      | p == P.rootDir → pure ""
      | otherwise → Left "Expected a file path"
    Just (Right p) → pure $ NES.toString $ un P.Name $ P.fileName p
  let props = maybe SM.empty (\(URI.QueryPairs qs) → SM.fromFoldable qs) query
  pure
    { host
    , bucketName
    , password: fromMaybe "" $ join (SM.lookup "password" props)
    , docTypeKey: fromMaybe "" $ join (SM.lookup "docTypeKey" props)
    , queryTimeout: map Seconds <<< Num.fromString =<< join (SM.lookup "queryTimeoutSeconds" props)
    }

uriScheme ∷ URI.Scheme
uriScheme = Scheme.unsafeFromString "couchbase"
