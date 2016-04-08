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

module Quasar.FS.Resource
  ( Resource(..)
  , fromJSON
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either(..))
import Data.Path.Pathy (AbsFile, AbsDir, Sandboxed, dir, file, (</>))

import Quasar.FS.Mount (Mount)
import Quasar.FS.Mount as Mount

data Resource
  = File (AbsFile Sandboxed)
  | Directory (AbsDir Sandboxed)
  | Mount Mount

derive instance eqResource ∷ Eq Resource

instance showResource ∷ Show Resource where
  show (File p) = "(File " <> show p <> ")"
  show (Directory p) = "(Directory " <> show p <> ")"
  show (Mount m) = "(Mount " <> show m <> ")"

fromJSON ∷ AbsDir Sandboxed → Json → Either String Resource
fromJSON parent json
  = Mount <$> Mount.fromJSON parent json
  <|> do
    obj ← decodeJson json
    typ ← obj .? "type"
    name ← obj .? "name"
    case typ of
      "directory" → Right $ Directory (parent </> dir name)
      "file" → Right $ File (parent </> file name)
      _ → Left $ "unknown resource type " ++ typ
