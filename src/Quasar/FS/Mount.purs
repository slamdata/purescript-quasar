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

module Quasar.FS.Mount where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Path.Pathy (DirName, FileName, dir, file, pathName, (</>))

import Quasar.Types (AnyPath, FilePath, DirPath)

data Mount
  = View FilePath
  | MongoDB DirPath

derive instance eqMount ∷ Eq Mount

instance showMount ∷ Show Mount where
  show (View p) = "(View " <> show p <> ")"
  show (MongoDB p) = "(MongoDB " <> show p <> ")"

-- | Attempts to decode a mount listing value from Quasar's filesystem metadata,
-- | for a mount in the specified parent directory.
fromJSON ∷ DirPath → Json → Either String Mount
fromJSON parent = decodeJson >=> \obj → do
  mount ← obj .? "mount"
  typ ← obj .? "type"
  name ← obj .? "name"
  case typ, mount of
    "file", "view" → Right $ View (parent </> file name)
    "directory", "mongodb" → Right $ MongoDB (parent </> dir name)
    _, _ → Left $
      "Unknown mount type '" <> mount <> "' for resource type '" <> typ <> "'"

getPath ∷ Mount → AnyPath
getPath (View p) = Right p
getPath (MongoDB p) = Left p

getName ∷ Mount → Either (Maybe DirName) FileName
getName = pathName <<< getPath
