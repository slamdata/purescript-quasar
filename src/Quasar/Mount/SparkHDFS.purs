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

module Quasar.Mount.SparkHDFS
  ( Config
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  ) where

import Prelude

import Data.Argonaut (Json, (.?), (:=), (~>))
import Data.Argonaut as J
import Data.Bifunctor (lmap)
import Data.Codec (decode, encode)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent, decodeURIComponent)
import Pathy (AbsDir)
import Quasar.Types (parseQDirPath, printQPath)
import Quasar.URI as URI
import URI.Scheme as Scheme

type Config =
  { sparkHost ∷ URI.QURIHost'
  , hdfsHost ∷ URI.QURIHost'
  , path ∷ AbsDir
  , props ∷ SM.StrMap (Maybe String)
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = encode URI.qAbsoluteURI (toURI config)
  in "spark-hdfs" := ("connectionUri" := uri ~> J.jsonEmptyObject) ~> J.jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< decode URI.qAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "spark-hdfs")
  <=< J.decodeJson

toURI ∷ Config → URI.QAbsoluteURI
toURI cfg =
  mkURI sparkURIScheme cfg.sparkHost (Just (URI.QueryPairs $ requiredProps <> optionalProps))
  where
  requiredProps ∷ Array (Tuple String (Maybe String))
  requiredProps =
    [ Tuple "hdfsUrl" $ Just $ encodeURIComponent $ encode URI.qAbsoluteURI $ mkURI hdfsURIScheme cfg.hdfsHost Nothing
    , Tuple "rootPath" $ Just $ printQPath cfg.path
    ]

  optionalProps ∷ Array (Tuple String (Maybe String))
  optionalProps = SM.toUnfoldable cfg.props

fromURI ∷ URI.QAbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI _ (URI.HierarchicalPartNoAuth _) _) = do
  Left "Expected 'auth' part in URI"
fromURI (URI.AbsoluteURI _ (URI.HierarchicalPartAuth (URI.Authority _ Nothing) _) _) = do
  Left "Expected 'host' part to be present in URL"
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPartAuth (URI.Authority _ (Just sparkHost)) _) query) = do
  unless (scheme == sparkURIScheme) $ Left "Expected `spark` URL scheme"
  let props = maybe SM.empty (\(URI.QueryPairs qs) → SM.fromFoldable qs) query

  Tuple hdfsHost props' ← case SM.pop "hdfsUrl" props of
    Just (Tuple (Just value) rest) → do
      value' ← extractHost' hdfsURIScheme $ decodeURIComponent value
      pure (Tuple value' rest)
    _ → Left "Expected `hdfsUrl` query parameter"

  Tuple path props'' ← case SM.pop "rootPath" props' of
    Just (Tuple (Just value) rest) → do
      dirPath ← note "Expected `rootPath` to be a directory path" $ parseQDirPath value
      pure (Tuple dirPath rest)
    _ → Left "Expected `rootPath` query parameter"

  pure { sparkHost, hdfsHost, path, props: props'' }

mkURI ∷ URI.Scheme → URI.QURIHost' → Maybe URI.QQuery → URI.QAbsoluteURI
mkURI scheme host params =
  URI.AbsoluteURI
    (scheme)
    (URI.HierarchicalPartAuth (URI.Authority Nothing (Just host)) Nothing)
    params

extractHost' ∷ URI.Scheme → String → Either String URI.QURIHost'
extractHost' scheme uri = do
  URI.AbsoluteURI scheme' hierPart _ ← lmap show $ decode URI.qAbsoluteURI uri
  unless (scheme' == scheme) $ Left $ "Expected '" <> Scheme.print scheme <> "' URL scheme"
  case hierPart of
    URI.HierarchicalPartNoAuth _ → Left $ "Expected auth part to be present in URL"
    URI.HierarchicalPartAuth (URI.Authority _ Nothing) _ → Left "Expected 'host' part to be present in URL"
    URI.HierarchicalPartAuth (URI.Authority _ (Just host)) _ → pure host

sparkURIScheme ∷ URI.Scheme
sparkURIScheme = Scheme.unsafeFromString "spark"

hdfsURIScheme ∷ URI.Scheme
hdfsURIScheme = Scheme.unsafeFromString "hdfs"
