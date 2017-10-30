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

module Quasar.Mount.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty ((:|))
import Quasar.Mount (MountConfig(..))
import Quasar.Mount.Couchbase.Gen as Couchbase
import Quasar.Mount.MarkLogic.Gen as MarkLogic
import Quasar.Mount.Mimir.Gen as Mimir
import Quasar.Mount.MongoDB.Gen as MongoDB
import Quasar.Mount.SparkHDFS.Gen as SparkHDFS
import Quasar.Mount.SparkLocal.Gen as SparkLocal
import Quasar.Mount.Unknown.Gen as Unknown
-- import Quasar.Mount.Module.Gen as Module
-- import Quasar.Mount.View.Gen as View

genMountConfig :: ∀ m. MonadGen m ⇒ MonadRec m ⇒ m MountConfig
genMountConfig = Gen.oneOf
  $ (MongoDBConfig <$> MongoDB.genConfig) :|
  [ CouchbaseConfig <$> Couchbase.genConfig
  , MarkLogicConfig <$> MarkLogic.genConfig
  , SparkHDFSConfig <$> SparkHDFS.genConfig
  , SparkLocalConfig <$> SparkLocal.genConfig
  , MimirConfig <$> Mimir.genConfig
  , UnknownConfig <$> Unknown.genConfig
-- TODO there is no MonadGen SqlModule or MonadGen SqlQuery
-- so we can't generate configurations. Once we have required
-- generators we should update this code
--   , ModuleConfig <$> Module.genConfig
--   , ViewConfig <$> View.genConfig
  ]
