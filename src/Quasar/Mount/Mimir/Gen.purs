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

module Quasar.Mount.Mimir.Gen where

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Quasar.Mount.Common.Gen (genDirPath)
import Quasar.Mount.Mimir as M

genConfig ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m M.Config
genConfig = genDirPath
