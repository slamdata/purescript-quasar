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

module Quasar.FS.Resource where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (fromString)
import Pathy (AbsDir, AbsFile, Dir, File, Name(..), AbsPath, dir', file', fileName, name, (</>))
import Quasar.FS.Mount as Mount

data QResource
  = File AbsFile
  | Directory AbsDir
  | Mount Mount.Mount

derive instance eqQResource ∷ Eq QResource
derive instance ordQResource ∷ Ord QResource

instance showQResource ∷ Show QResource where
  show (File p) = "(File " <> show p <> ")"
  show (Directory p) = "(Directory " <> show p <> ")"
  show (Mount m) = "(Mount " <> show m <> ")"

fromJSON ∷ AbsDir → Json → Either String QResource
fromJSON parent json
  = Mount <$> Mount.fromJSON parent json
  <|> do
    obj ← decodeJson json
    name' ← note "empty name" <<< fromString =<< (obj .? "name")
    obj .? "type" >>= case _ of
      "directory" → Right $ Directory (parent </> dir' (Name name'))
      "file" → Right $ File (parent </> file' (Name name'))
      typ → Left $ "unknown resource type " <> typ

getPath ∷ QResource → AbsPath
getPath (File p) = Right p
getPath (Directory p) = Left p
getPath (Mount m) = Mount.getPath m

getName ∷ QResource → Either (Maybe (Name Dir)) (Name File)
getName = bimap name fileName <<< getPath
