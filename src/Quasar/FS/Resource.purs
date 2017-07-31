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
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Path.Pathy (DirName, FileName, dir, file, pathName, (</>))
import Quasar.FS.Mount as Mount
import Quasar.Types (AnyPath, FilePath, DirPath)

data Resource
  = File FilePath
  | Directory DirPath
  | Mount Mount.Mount

derive instance eqResource ∷ Eq Resource

instance showResource ∷ Show Resource where
  show (File p) = "(File " <> show p <> ")"
  show (Directory p) = "(Directory " <> show p <> ")"
  show (Mount m) = "(Mount " <> show m <> ")"

fromJSON ∷ DirPath → Json → Either String Resource
fromJSON parent json
  = Mount <$> Mount.fromJSON parent json
  <|> do
    obj ← decodeJson json
    name ← obj .? "name"
    obj .? "type" >>= case _ of
      "directory" → Right $ Directory (parent </> dir name)
      "file" → Right $ File (parent </> file name)
      typ → Left $ "unknown resource type " <> typ

getPath ∷ Resource → AnyPath
getPath (File p) = Right p
getPath (Directory p) = Left p
getPath (Mount m) = Mount.getPath m

getName ∷ Resource → Either (Maybe DirName) FileName
getName = pathName <<< getPath
