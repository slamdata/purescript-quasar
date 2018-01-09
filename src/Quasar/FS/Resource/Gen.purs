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

module Quasar.FS.Resource.Gen where


import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Quasar.FS.Mount.Gen (genMount)
import Quasar.FS.Resource (QResource(..))
import Quasar.Mount.Common.Gen (genAbsDirPath, genAbsFilePath)


genQResource :: ∀ m. MonadGen m ⇒ MonadRec m ⇒ m QResource
genQResource = Gen.frequency
  $ (Tuple 1.0 $ genAbsFilePath <#> File ) :|
  [ Tuple 1.0 $ genAbsDirPath <#> Directory 
  , Tuple 3.0 $ genMount <#> Mount
  ]
