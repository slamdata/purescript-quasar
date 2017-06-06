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

module Quasar.Mount.MarkLogic.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Quasar.Mount.Common.Gen (genAlphaNumericString, genHost, genAnyPath)
import Quasar.Mount.MarkLogic as ML

genFormat ∷ ∀ m. MonadGen m ⇒ m ML.Format
genFormat = Gen.choose (pure ML.JSON) (pure ML.XML)

genConfig ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m ML.Config
genConfig = do
  host ← genHost
  path ← GenC.genMaybe genAnyPath
  withCreds ← Gen.chooseBool
  format ← genFormat
  case withCreds of
    true → do
      user ← Just <$> genAlphaNumericString
      password ← Just <$> genAlphaNumericString
      pure { host, path, user, password, format }
    false →
      pure { host, path, user: Nothing, password: Nothing, format }
