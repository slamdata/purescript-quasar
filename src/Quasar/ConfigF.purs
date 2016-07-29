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

module Quasar.ConfigF where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.Monad.State.Class (class MonadState, gets)

data ConfigF c a = GetConfig (c → a)

evalReader ∷ ∀ c m. MonadReader c m ⇒ ConfigF c ~> m
evalReader (GetConfig k) = k <$> ask

evalState ∷ ∀ c m. MonadState c m ⇒ ConfigF c ~> m
evalState (GetConfig k) = gets k
