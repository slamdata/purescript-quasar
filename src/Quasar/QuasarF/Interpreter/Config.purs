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

module Quasar.QuasarF.Interpreter.Config where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsDir, RelDir, Sandboxed, Unsandboxed)
import Data.URI (Authority, URIScheme)

type AbsBasePath = { scheme ∷ URIScheme, authority ∷ Maybe Authority, path ∷ AbsDir Sandboxed }
type BasePath = Either AbsBasePath (RelDir Unsandboxed)

type Config r = { basePath ∷ BasePath | r }
