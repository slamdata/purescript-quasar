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

module Test.Property.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Quasar.Mount.MongoDB as MDB
import Test.Property.Mount.Couchbase as Quasar.Mount.Couchbase
import Test.Property.Mount.MarkLogic as Quasar.Mount.MarkLogic
import Test.Property.Mount.Mimir as Quasar.Mount.Mimir
import Test.Property.Mount.MongoDB as Quasar.Mount.MongoDB
import Test.Property.Mount.SparkFTP as Quasar.Mount.SparkFTP
import Test.Property.Mount.SparkHDFS as Quasar.Mount.SparkHDFS
import Test.Property.Mount.SparkLocal as Quasar.Mount.SparkLocal
import Test.QuickCheck (QC)

newtype TestConfig = TestConfig MDB.Config

derive instance eqTestConfig ∷ Eq TestConfig

instance showTestConfig ∷ Show TestConfig where
  show (TestConfig cfg) = show (MDB.toJSON cfg)

main ∷ ∀ eff. QC eff Unit
main = do

  log "Check Quasar.Mount.Couchbase..."
  Quasar.Mount.Couchbase.check

  log "Check Quasar.Mount.MarkLogic..."
  Quasar.Mount.MarkLogic.check

  log "Check Quasar.Mount.Mimir..."
  Quasar.Mount.Mimir.check

  log "Check Quasar.Mount.MongoDB..."
  Quasar.Mount.MongoDB.check

  log "Check Quasar.Mount.SparkFTP..."
  Quasar.Mount.SparkFTP.check

  log "Check Quasar.Mount.SparkHDFS..."
  Quasar.Mount.SparkHDFS.check

  log "Check Quasar.Mount.SparkLocal..."
  Quasar.Mount.SparkLocal.check
