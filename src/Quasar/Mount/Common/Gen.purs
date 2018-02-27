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
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String as S
import Data.String.Gen as SG
import Data.String.NonEmpty (NonEmptyString, cons)
import Data.These (These(..))
import Data.URI.Host.IPv4Address (fromInts) as IPv4Address
import Pathy.Gen (genAbsDirPath, genAbsFilePath) as PGen
import Quasar.Data.URI as URI
import Quasar.Types (AnyPath)

genAlphaNumericString ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m String
genAlphaNumericString = SG.genString genAlphaNumericChar

genAlphaNumericNEString ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m NonEmptyString
genAlphaNumericNEString = cons <$> genAlphaNumericChar <*> SG.genString genAlphaNumericChar

genAlphaNumericChar ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Char
genAlphaNumericChar = Gen.oneOf $ CG.genDigitChar :| [CG.genAlpha]

genHostURI ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.Host
genHostURI = Gen.oneOf $ genIPv4 :| [genName]
  where
  genIPv4 = filtered do
    a ← Gen.chooseInt 1 254
    b ← Gen.chooseInt 1 254
    c ← Gen.chooseInt 1 254
    d ← Gen.chooseInt 1 254
    pure $ URI.IPv4Address <$> IPv4Address.fromInts a b c d
  genName = URI.NameAddress <$> genRegName
  genRegName = filtered do
    head ← S.singleton <$> CG.genAlpha
    tail ← genAlphaNumericString
    pure $ URI.regNameFromString $ head <> tail

genPort ∷ ∀ m. MonadRec m => MonadGen m ⇒ m URI.Port
genPort = filtered $ URI.portFromInt <$> Gen.chooseInt 50000 65535

genHost ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.QURIHost
genHost = Gen.unfoldable $ genThese genHostURI genPort

genThese ∷ ∀ m a b. MonadGen m ⇒ MonadRec m ⇒ m a -> m b -> m (These a b)
genThese ma mb = filtered do
  a' <- GenC.genMaybe ma
  b' <- GenC.genMaybe mb
  pure case a', b' of
    Just a, Just b -> Just $ Both a b
    Just a, Nothing -> Just $ This a
    Nothing, Just b -> Just $ That b
    Nothing, Nothing -> Nothing

genCredentials ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.UserPassInfo
genCredentials =
  URI.UserPassInfo <$> ({ user: _, password: _ }
    <$> genAlphaNumericString
    <*> Gen.choose (pure Nothing) (Just <$> genAlphaNumericString))

genAnyPath ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m AnyPath
genAnyPath = Gen.oneOf $ (Left <$> PGen.genAbsDirPath) :| [Right <$> PGen.genAbsFilePath]
