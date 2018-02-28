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

module Quasar.FS.Mount.Gen where


import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Const (Const(..))
import Data.Identity (Identity(..))
import Data.NonEmpty ((:|))
import Data.String.Gen (genUnicodeString)
import Pathy.Gen (genAbsAnyPath)
import Quasar.FS.Mount (MountF(..), Mount, MountType)
import Quasar.Mount.Common.Gen (genAbsDirPath, genAbsFilePath)


genMountType ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m MountType
genMountType = Gen.oneOf
  $ (pure $ View $ Const unit) :|
  [ pure $ Module $ Const unit
  , pure $ MongoDB $ Const unit
  , pure $ Couchbase $ Const unit
  , pure $ MarkLogic $ Const unit
  , pure $ SparkHDFS $ Const unit
  , pure $ SparkLocal $ Const unit
  , pure $ Mimir $ Const unit
  , genUnicodeString <#> (_ `Unknown` Const unit)
  ]

genMount ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Mount
genMount = Gen.oneOf
  $ (genAbsFilePath <#> Identity >>> View) :|
  [ genAbsDirPath <#> Identity >>> Module
  , genAbsDirPath <#> Identity >>> MongoDB
  , genAbsDirPath <#> Identity >>> Couchbase
  , genAbsDirPath <#> Identity >>> MarkLogic
  , genAbsDirPath <#> Identity >>> SparkHDFS
  , genAbsDirPath <#> Identity >>> SparkLocal
  , genAbsDirPath <#> Identity >>> Mimir
  , Unknown <$> genUnicodeString <*> map Identity genAbsAnyPath
  ]
