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

module Quasar.Advanced.QuasarAF.Interpreter.Internal where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Data.String as String
import Data.URI as URI
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Quasar.Advanced.Paths as Paths
import Quasar.Advanced.QuasarAF (GroupPath(..))
import Quasar.Advanced.QuasarAF.Interpreter.Config (Config)
import Quasar.ConfigF as CF
import Quasar.QuasarF.Interpreter.Internal (mkFSUrl)

type AXFP = AXF.AffjaxFP RequestContent String

mkGroupUrl
  ∷ ∀ r
  . GroupPath
  → URI.Query
  → Free (Coproduct (CF.ConfigF (Config r)) AXFP) String
mkGroupUrl (GroupPath gp) q = do
  url ← mkFSUrl Paths.group (Left gp) q
  pure case String.stripSuffix (String.Pattern"/") url of
    -- This is necessary because GroupPaths need to be rendered as files, but
    -- the root group can't be represented as a file -.-
    Just url' | gp /= rootDir → url'
    _ → url
