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

module Quasar.Mount.Mimir
  ( Config
  , toJSON
  , fromJSON
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either, note)
import Pathy (Abs, Dir, Path)
import Quasar.Types (parseQDirPath, printQPath)

type Config = Path Abs Dir

toJSON ∷ Config → Json
toJSON config =
  "mimir" := ("connectionUri" := printQPath config ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = note "Couldn't parse absolute dir path"
  <<< parseQDirPath
  <=< (_ .? "connectionUri")
  <=< (_ .? "mimir")
  <=< decodeJson
