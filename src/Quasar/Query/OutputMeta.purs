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

module Quasar.Query.OutputMeta where

import Prelude

import Data.Argonaut (Json, JArray, decodeJson, (.?))
import Data.Either (Either, note)
import Pathy (AbsFile)
import Quasar.Types (parseQFilePath)

type OutputMeta =
  { out ∷ AbsFile
  , phases ∷ JArray
  }

fromJSON ∷ Json → Either String OutputMeta
fromJSON json = do
  obj ← decodeJson json
  path ← obj .? "out"
  out ← note "Could not parse 'out' path" $ parseQFilePath path
  phases ← obj .? "phases"
  pure { out, phases }
