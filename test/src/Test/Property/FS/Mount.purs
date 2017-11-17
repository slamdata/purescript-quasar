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

module Test.Property.FS.Mount where

import Prelude

import Quasar.FS.Mount (MountType)
import Quasar.FS.Mount.Gen as GenM
import Test.QuickCheck (QC, class Arbitrary)
import Test.QuickCheck.Laws as Laws
import Test.QuickCheck.Laws.Data as Data
import Type.Prelude (Proxy(..))

newtype ArbMountType = ArbMountType MountType

instance arbitraryArbMountType ∷ Arbitrary ArbMountType where
  arbitrary = ArbMountType <$> GenM.genMountType

derive newtype instance eqArbMountType ∷ Eq ArbMountType
derive newtype instance ordArbMountType ∷ Ord ArbMountType

check ∷ ∀ eff. QC eff Unit
check = Laws.checkLaws "MountType" do
  Data.checkEq prxMountType
  Data.checkOrd prxMountType
  where
  prxMountType = Proxy ∷ Proxy ArbMountType
