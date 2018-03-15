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

module Quasar.Mount.Common.Gen
  ( module Quasar.Mount.Common.Gen
  , module PGen
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, filtered)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common (genMaybe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String.Gen as SG
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.These.Gen (genThese)
import Pathy.Gen (genAbsDirPath, genAbsFilePath) as PGen
import Quasar.URI as URI
import URI.Host.Gen as HostGen
import URI.Port as Port

genAlphaNumericString ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m String
genAlphaNumericString = SG.genString genAlphaNumericChar

genAlphaNumericNEString ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m NonEmptyString
genAlphaNumericNEString = NES.cons <$> genAlphaNumericChar <*> SG.genString genAlphaNumericChar

genAlphaNumericChar ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Char
genAlphaNumericChar = Gen.oneOf $ CG.genDigitChar :| [CG.genAlpha]

genPort ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m URI.Port
genPort = filtered $ Port.fromInt <$> Gen.chooseInt 50000 65535

genHost' ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.QURIHost'
genHost' = genThese HostGen.genHost genPort

genHost ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.QURIHost
genHost = genMaybe genHost'

genHosts ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.QURIHosts
genHosts = Gen.unfoldable genHost'

genCredentials ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.UserPassInfo
genCredentials =
  URI.UserPassInfo <$> ({ user: _, password: _ }
    <$> genAlphaNumericNEString
    <*> Gen.choose (pure Nothing) (Just <$> genAlphaNumericNEString))
