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

module Test.Property.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Quasar.Mount.MongoDB as MDB
import Test.Property.Mount.MongoDB as Quasar.Mount.MongoDB
import Test.StrongCheck (SC)

newtype TestConfig = TestConfig MDB.Config

derive instance eqTestConfig ∷ Eq TestConfig

instance showTestConfig ∷ Show TestConfig where
  show (TestConfig cfg) = show (MDB.toJSON cfg)

main ∷ ∀ eff. SC eff Unit
main = do
  log "Check Quasar.Mount.MongoDB..."
  Quasar.Mount.MongoDB.check
