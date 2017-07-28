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

module Test.Property.Mount.Mimir where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Quasar.Mount.Mimir as SL
import Quasar.Mount.Mimir.Gen (genConfig)
import Test.QuickCheck (QC, Result, quickCheck, (===))
import Test.QuickCheck.Gen (Gen)

newtype TestConfig = TestConfig SL.Config

derive instance eqTestConfig ∷ Eq TestConfig
derive instance genericTestConfig ∷ Generic TestConfig _
instance showTestConfig ∷ Show TestConfig where show = genericShow

check ∷ ∀ eff. QC eff Unit
check = quickCheck prop
  where
  prop ∷ Gen Result
  prop = do
    configIn ← genConfig
    let configOut = SL.fromJSON (SL.toJSON configIn)
    pure $ Right (TestConfig configIn) === TestConfig <$> configOut
