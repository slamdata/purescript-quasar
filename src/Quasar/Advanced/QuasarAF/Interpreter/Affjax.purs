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

module Quasar.Advanced.QuasarAF.Interpreter.Affjax (eval) where

import Prelude

import Control.Monad.Free (Free, mapF)

import Data.Functor.Coproduct (Coproduct, left, right, coproduct)
import Data.NaturalTransformation (Natural)
import Data.Path.Pathy (printPath)
import Data.String as Str

import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req

import Quasar.Advanced.Paths as Paths
import Quasar.Advanced.QuasarAF (QuasarAF(..))
import Quasar.ConfigF as CF
import Quasar.QuasarF (QuasarF)
import Quasar.QuasarF.Interpreter.Affjax as QCI
import Quasar.QuasarF.Interpreter.Config (Config)
import Quasar.QuasarF.Interpreter.Internal (ask, mkRequest, jsonResult, get)

type M = Free (Coproduct (CF.ConfigF Config) (AXF.AffjaxFP RequestContent String))

eval ∷ Natural (Coproduct QuasarF QuasarAF) M
eval = coproduct (mapF (coproduct left (right <<< authify)) <<< QCI.eval) evalA

evalA ∷ Natural QuasarAF M
evalA = \q -> case q of

  GetAuthProviders k -> do
    { basePath } ← ask
    k <$> mkRequest jsonResult (get (basePath <> Str.drop 1 (printPath Paths.oidcProviders)))

authify ∷ Natural (AXF.AffjaxFP RequestContent String) (AXF.AffjaxFP RequestContent String)
authify (AXF.AffjaxFP req k) =
  AXF.AffjaxFP (req { headers = req.headers <> [Req.RequestHeader "X-Test" "Test"] }) k
