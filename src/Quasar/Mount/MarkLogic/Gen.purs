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

module Quasar.Mount.MarkLogic.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Pathy.Gen (genAbsAnyPath)
import Quasar.Mount.Common.Gen (genCredentials, genHost)
import Quasar.Mount.MarkLogic as ML

genFormat ∷ ∀ m. MonadGen m ⇒ m ML.Format
genFormat = Gen.choose (pure ML.JSON) (pure ML.XML)

genConfig ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m ML.Config
genConfig =
  { host: _, path: _, credentials: _, format: _ }
    <$> genHost
    <*> GenC.genMaybe genAbsAnyPath
    <*> GenC.genMaybe genCredentials
    <*> genFormat
