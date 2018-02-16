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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy (parseAbsDir, printPath, rootDir, sandbox, (</>))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Global (encodeURIComponent, decodeURIComponent)
import Quasar.Data.URI as URI
import Quasar.Types (DirPath)
import Text.Parsing.Parser (runParser)

type Config =
  { sparkHost ∷ URI.QURIHost
  , hdfsHost ∷ URI.QURIHost
  , path ∷ DirPath
  , props ∷ SM.StrMap (Maybe String)
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.qAbsoluteURI.print (toURI config)
  in "spark-hdfs" := ("connectionUri" := uri ~> J.jsonEmptyObject) ~> J.jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< flip runParser URI.qAbsoluteURI.parser
  <=< (_ .? "connectionUri")
  <=< (_ .? "spark-hdfs")
  <=< J.decodeJson

toURI ∷ Config → URI.QAbsoluteURI
toURI cfg =
  mkURI sparkURIScheme cfg.sparkHost (Just (URI.QueryPairs $ requiredProps <> optionalProps))
  where
  requiredProps ∷ Array (Tuple String (Maybe String))
  requiredProps = 
    [ Tuple "hdfsUrl" $ Just $ encodeURIComponent $ URI.qAbsoluteURI.print $ mkURI hdfsURIScheme cfg.hdfsHost Nothing
    , Tuple "rootPath" $ Just $ printPath cfg.path
    ]

  optionalProps ∷ Array (Tuple String (Maybe String))
  optionalProps = SM.toUnfoldable cfg.props

fromURI ∷ URI.QAbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI _ (URI.HierarchicalPartNoAuth _) _) = do
  Left "Expected 'auth' part in URI"
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPartAuth (URI.Authority _ sparkHost) _) query) = do
  unless (scheme == sparkURIScheme) $ Left "Expected `spark` URL scheme"
  let props = maybe SM.empty (\(URI.QueryPairs qs) → SM.fromFoldable qs) query

  Tuple hdfsHost props' ← case SM.pop "hdfsUrl" props of
    Just (Tuple (Just value) rest) → do
      value' ← extractHost' hdfsURIScheme $ decodeURIComponent value
      pure (Tuple value' rest)
    _ → Left "Expected `hdfsUrl` query parameter"

  Tuple path props'' ← case SM.pop "rootPath" props' of
    Just (Tuple (Just value) rest) → do
      dirPath ← case parseAbsDir value >>= sandbox rootDir of
        Just dp → pure $ rootDir </> dp
        Nothing → Left "Expected `rootPath` to be a directory path"
      pure (Tuple dirPath rest)
    _ → Left "Expected `rootPath` query parameter"

  pure { sparkHost, hdfsHost, path, props: props'' }

mkURI :: URI.Scheme -> URI.QURIHost -> Maybe URI.QQuery -> URI.QAbsoluteURI
mkURI scheme host params =
  URI.AbsoluteURI
    (scheme)
    (URI.HierarchicalPartAuth (URI.Authority Nothing host) Nothing)
    params


extractHost' ∷ URI.Scheme → String → Either String URI.QURIHost
extractHost' scheme uri = do
  URI.AbsoluteURI scheme' hierPart _ ← lmap show $ runParser uri URI.qAbsoluteURI.parser
  unless (scheme' == scheme) $ Left $ "Expected '" <> URI.printScheme scheme <> "' URL scheme"
  case hierPart of
    URI.HierarchicalPartNoAuth _ -> Left $ "Expected auth part to be present in URL"
    URI.HierarchicalPartAuth (URI.Authority _ host) _ -> pure host

sparkURIScheme ∷ URI.Scheme
sparkURIScheme = URI.unsafeSchemaFromString "spark"

hdfsURIScheme ∷ URI.Scheme
hdfsURIScheme = URI.unsafeSchemaFromString "hdfs"
