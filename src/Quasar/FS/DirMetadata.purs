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

module Quasar.FS.DirMetadata
  ( DirMetadata
  , fromJSON
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)
import Data.Traversable (traverse)
import Pathy (AbsDir)
import Quasar.FS.Resource (QResource)
import Quasar.FS.Resource as QResource

type DirMetadata = Array QResource

fromJSON ∷ AbsDir → Json → Either String DirMetadata
fromJSON parent json = do
  obj ← decodeJson json
  children ← obj .? "children"
  traverse (QResource.fromJSON parent) children
