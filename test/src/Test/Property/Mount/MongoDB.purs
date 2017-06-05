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

module Test.Property.Mount.MongoDB where

import Prelude

import Data.Either (Either(..))
import Quasar.Mount.MongoDB as MDB
import Quasar.Mount.MongoDB.Gen (genConfig)
import Test.StrongCheck (SC, Result, quickCheck, (===))
import Test.StrongCheck.Gen (Gen)

newtype TestConfig = TestConfig MDB.Config

derive instance eqTestConfig ∷ Eq TestConfig

instance showTestConfig ∷ Show TestConfig where
  show (TestConfig cfg) = show (MDB.toJSON cfg)

check ∷ ∀ eff. SC eff Unit
check = (quickCheck ∷ Gen Result → SC eff Unit) do
  configIn ← genConfig
  let configOut = MDB.fromJSON (MDB.toJSON configIn)
  pure $ Right (TestConfig configIn) === TestConfig <$> configOut
