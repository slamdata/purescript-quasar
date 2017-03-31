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

module Quasar.Mount.SparkHDFS
  ( Config
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  , module Exports
  ) where

import Prelude

import Data.Argonaut (Json, (.?), (:=), (~>))
import Data.Argonaut as J
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Data.URI.Path (printPath, parseURIPathAbs)

import Global (encodeURIComponent, decodeURIComponent)

import Quasar.Mount.Common (Host, extractHost)
import Quasar.Mount.Common (Host) as Exports
import Quasar.Types (AnyPath)

import Text.Parsing.StringParser (runParser)

type Config =
  { sparkHost ∷ Host
  , hdfsHost ∷ Host
  , path ∷ Maybe AnyPath
  , executorMemory ∷ Maybe String
  , executorCores ∷ Maybe Int
  , executorExtraJavaOptions ∷ Maybe String
  , defaultParallelism ∷ Maybe Int
  , filesMaxPartitionBytes ∷ Maybe Number
  , driverCores ∷ Maybe Int
  , driverMaxResultSize ∷ Maybe String
  , driverMemory ∷ Maybe String
  , localDir ∷ Maybe String
  , reducerMaxSizeInFlight ∷ Maybe String
  , reducerMaxReqsInFlight ∷ Maybe Int
  , shuffleFileBuffer ∷ Maybe String
  , shuffleIoRetryWait ∷ Maybe String
  , memoryFraction ∷ Maybe Number
  , memoryStorageFraction ∷ Maybe Number
  , coresMax ∷ Maybe Number
  , speculation ∷ Maybe Boolean
  , tasksCpus ∷ Maybe Int
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.printAbsoluteURI (toURI config)
  in "spark-hdfs" := ("connectionUri" := uri ~> J.jsonEmptyObject) ~> J.jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< URI.runParseAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "spark")
  <=< J.decodeJson

toJSONString ∷ ∀ a. J.EncodeJson a ⇒ a → String
toJSONString = J.printJson <<< J.encodeJson

fromJSONString ∷ ∀ a. J.DecodeJson a ⇒ String → Either String a
fromJSONString = J.decodeJson <=< J.jsonParser

toURI ∷ Config → URI.AbsoluteURI
toURI cfg = mkURI sparkURIScheme cfg.sparkHost (Just (URI.Query props))
  where
  props ∷ L.List (Tuple String (Maybe String))
  props = L.mapMaybe (map (map Just)) $ L.fromFoldable
    [ Tuple "hdfsUrl" <$> Just (encodeURIComponent $ URI.printAbsoluteURI $ mkURI hdfsURIScheme cfg.hdfsHost Nothing)
    , Tuple "rootPath" <$> Just (maybe "/" printPath cfg.path)
    , Tuple "spark.executor.memory" <$> cfg.executorMemory
    , Tuple "spark.executor.cores" <<< toJSONString <$> cfg.executorCores
    , Tuple "spark.executor.extraJavaOptions" <$> cfg.executorExtraJavaOptions
    , Tuple "spark.default.parallelism" <<< toJSONString <$> cfg.defaultParallelism
    , Tuple "spark.files.maxPartitionBytes" <<< toJSONString <$> cfg.filesMaxPartitionBytes
    , Tuple "spark.driver.cores" <<< toJSONString <$> cfg.driverCores
    , Tuple "spark.driver.maxResultSize" <$> cfg.driverMaxResultSize
    , Tuple "spark.driver.memory" <$> cfg.driverMemory
    , Tuple "spark.local.dir" <$> cfg.localDir
    , Tuple "spark.reducer.maxSizeInFlight" <$> cfg.reducerMaxSizeInFlight
    , Tuple "spark.reducer.maxReqsInFlight" <<< toJSONString <$> cfg.reducerMaxReqsInFlight
    , Tuple "spark.shuffle.file.buffer" <$> cfg.shuffleFileBuffer
    , Tuple "spark.shuffle.io.retryWait" <$> cfg.shuffleIoRetryWait
    , Tuple "spark.memory.fraction" <<< toJSONString <$> cfg.memoryFraction
    , Tuple "spark.memory.storageFraction" <<< toJSONString <$> cfg.memoryStorageFraction
    , Tuple "spark.cores.max" <<< toJSONString <$> cfg.coresMax
    , Tuple "spark.speculation" <<< toJSONString <$> cfg.speculation
    , Tuple "spark.tasks.cpus" <<< toJSONString <$> cfg.tasksCpus
    ]

  mkURI ∷ URI.URIScheme → Host → Maybe URI.Query → URI.AbsoluteURI
  mkURI scheme host params =
    URI.AbsoluteURI
      (Just scheme)
      (URI.HierarchicalPart (Just (URI.Authority Nothing (pure host))) Nothing)
      params

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth _) query) = do
  unless (scheme == Just sparkURIScheme) $ Left "Expected `spark` URL scheme"
  sparkHost ← extractHost auth

  let
    props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query
    lookup = join <<< flip SM.lookup props
    lookupJSON ∷ ∀ a. J.DecodeJson a ⇒ String → Either String (Maybe a)
    lookupJSON = traverse fromJSONString <<< lookup

  hdfsHost ← lookup "hdfsUrl" # maybe
    (Left "Expected `hdfsUrl` query parameter")
    (extractHost' hdfsURIScheme <<< decodeURIComponent)

  path ← lookup "rootPath" # maybe
    (Left "Expected `rootPath` query parameter")
    (bimap show Just <<< runParser parseURIPathAbs)

  let
    executorMemory = lookup "spark.executor.memory"
    executorExtraJavaOptions = lookup "spark.executor.extraJavaOptions"
    driverMaxResultSize = lookup "spark.driver.maxResultSize"
    driverMemory = lookup "spark.driver.memory"
    localDir = lookup "spark.local.dir"
    reducerMaxSizeInFlight = lookup "spark.reducer.maxSizeInFlight"
    shuffleFileBuffer = lookup "spark.shuffle.file.buffer"
    shuffleIoRetryWait = lookup "spark.shuffle.io.retryWait"

  executorCores ← lookupJSON "spark.executor.cores"
  defaultParallelism ← lookupJSON "spark.default.parallelism"
  filesMaxPartitionBytes ← lookupJSON "spark.files.maxPartitionBytes"
  driverCores ← lookupJSON "spark.driver.cores"
  reducerMaxReqsInFlight ← lookupJSON "spark.reducer.maxReqsInFlight"
  memoryFraction ← lookupJSON "spark.memory.fraction"
  memoryStorageFraction ← lookupJSON "spark.memory.storageFraction"
  coresMax ← lookupJSON "spark.cores.max"
  speculation ← lookupJSON "spark.speculation"
  tasksCpus ← lookupJSON "spark.tasks.cpus"

  pure
    { sparkHost
    , hdfsHost
    , path
    , executorMemory
    , executorCores
    , executorExtraJavaOptions
    , defaultParallelism
    , filesMaxPartitionBytes
    , driverCores
    , driverMaxResultSize
    , driverMemory
    , localDir
    , reducerMaxSizeInFlight
    , reducerMaxReqsInFlight
    , shuffleFileBuffer
    , shuffleIoRetryWait
    , memoryFraction
    , memoryStorageFraction
    , coresMax
    , speculation
    , tasksCpus
    }

extractHost' ∷ URI.URIScheme → String → Either String Host
extractHost' scheme@(URI.URIScheme name) uri = do
  URI.AbsoluteURI scheme' (URI.HierarchicalPart auth _) _ ←
    lmap show $ URI.runParseAbsoluteURI uri
  unless (scheme' == Just scheme) $ Left $ "Expected '" <> name <> "' URL scheme"
  extractHost auth

sparkURIScheme ∷ URI.URIScheme
sparkURIScheme = URI.URIScheme "spark"

hdfsURIScheme ∷ URI.URIScheme
hdfsURIScheme = URI.URIScheme "hdfs"
