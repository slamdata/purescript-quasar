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
  , module Exports
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (un)
import Data.Number as Num
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.StrMap as SM
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Data.URI.AbsoluteURI as AbsoluteURI
import Quasar.Mount.Common (Host) as Exports
import Quasar.Mount.Common (Host, extractHost)

type Config =
  { host ∷ Host
  , bucketName ∷ String
  , password ∷ String
  , docTypeKey ∷ String
  , queryTimeout ∷ Maybe Seconds
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = AbsoluteURI.print (toURI config)
  in "couchbase" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< AbsoluteURI.parse
  <=< (_ .? "connectionUri")
  <=< (_ .? "couchbase")
  <=< decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { host, bucketName, password, docTypeKey, queryTimeout } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart
      (Just (URI.Authority Nothing (pure host)))
      (Just (Right (P.rootDir </> P.file bucketName))))
    (Just (URI.Query props))
  where
  props ∷ L.List (Tuple String (Maybe String))
  props = L.Nil
    <> pure (Tuple "password" (Just password))
    <> pure (Tuple "docTypeKey" (Just docTypeKey))
    <> maybe L.Nil (pure <<< Tuple "queryTimeoutSeconds" <<< Just <<< show <<< un Seconds) queryTimeout

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth path) query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'couchbase' URL scheme"
  host ← extractHost auth
  bucketName ← case path of
    Nothing → Left "Path is missing from URL"
    Just (Left p)
      | p == P.rootDir → pure ""
      | otherwise → Left "Expected a file path"
    Just (Right p) → pure $ P.runFileName $ P.fileName p
  let props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query
  pure
    { host
    , bucketName
    , password: fromMaybe "" $ join (SM.lookup "password" props)
    , docTypeKey: fromMaybe "" $ join (SM.lookup "docTypeKey" props)
    , queryTimeout: map Seconds <<< Num.fromString =<< join (SM.lookup "queryTimeoutSeconds" props)
    }

uriScheme ∷ URI.Scheme
uriScheme = URI.Scheme "couchbase"
