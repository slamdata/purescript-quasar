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
  , parseDirPath
  , module Exports
  ) where

import Prelude
import Data.Path.Pathy as P
import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Path.Pathy (Abs, Dir, Path, Sandboxed, Unsandboxed, (</>))
import Quasar.Mount.Common (Host) as Exports

type Config = Path Abs Dir Sandboxed

sandbox
  ∷ forall a
  . Path Abs a Unsandboxed
  → Maybe (Path Abs a Sandboxed)
sandbox =
  map (P.rootDir </> _) <<< P.sandbox P.rootDir

parseDirPath ∷ String -> Maybe (Path Abs Dir Sandboxed)
parseDirPath = sandbox <=< P.parseAbsDir

toJSON ∷ Config → Json
toJSON config =
  let uri = P.printPath config
  in "mimir" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = maybe (Left "Couldn't sandbox") Right
  <<< parseDirPath
  <=< (_ .? "connectionUri")
  <=< (_ .? "mimir")
  <=< decodeJson
