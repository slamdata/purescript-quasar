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

module Quasar.Mount.Common.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as L
import Data.NonEmpty ((:|))
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String as S
import Data.String.Gen as SG
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Quasar.Mount.Common (Credentials(..))
import Quasar.Mount.MongoDB as MDB
import Quasar.Types (AnyPath, DirPath, FilePath)

genAlphaNumericString ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m String
genAlphaNumericString = SG.genString $ Gen.oneOf $ CG.genDigitChar :| [CG.genAlpha]

genHostURI ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m URI.Host
genHostURI = Gen.oneOf $ genIPv4 :| [genName]
  where
  genIPv4 = do
    a ← Gen.chooseInt 1 254
    b ← Gen.chooseInt 1 254
    c ← Gen.chooseInt 1 254
    d ← Gen.chooseInt 1 254
    pure $ URI.IPv4Address $ S.joinWith "." $ show <$> [a, b, c, d]
  genName = do
    head ← S.singleton <$> CG.genAlpha
    tail ← genAlphaNumericString
    pure $ URI.NameAddress $ head <> tail

genPort ∷ ∀ m. MonadGen m ⇒ m URI.Port
genPort = Gen.chooseInt 50000 65535

genHost ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m MDB.Host
genHost = Tuple <$> genHostURI <*> GenC.genMaybe genPort

genCredentials ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Credentials
genCredentials =
  Credentials <$> ({ user: _, password: _ }
    <$> genAlphaNumericString
    <*> Gen.choose (pure "") genAlphaNumericString)

genDirPath ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m DirPath
genDirPath = Gen.sized \size → do
  newSize <- Gen.chooseInt 0 size
  Gen.resize (const newSize) do
    parts ∷ L.List String ← Gen.unfoldable genAlphaNumericString
    pure $ foldr (flip P.appendPath <<< P.dir) P.rootDir parts

genFilePath ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m FilePath
genFilePath = do
  dir ← genDirPath
  file ← genAlphaNumericString
  pure $ dir </> P.file file

genAnyPath ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m AnyPath
genAnyPath = Gen.oneOf $ (Left <$> genDirPath) :| [Right <$> genFilePath]
