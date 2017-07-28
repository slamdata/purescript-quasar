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

module Quasar.Mount.Unknown where

import Prelude

import Data.Argonaut as J
import Data.Argonaut ((:=), (~>))
import Data.Array as A
import Data.Either (Either, note)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

type Config = { mountType ∷ String, connectionUri ∷ String }

toJSON ∷ Config → J.Json
toJSON { mountType, connectionUri } =
  mountType := ("connectionUri" := connectionUri ~> J.jsonEmptyObject) ~> J.jsonEmptyObject

fromJSON ∷ J.Json → Either String Config
fromJSON json = do
  obj ← note "Expected config object" $ J.toObject json
  Tuple mountType inner ← note "Expected config object to have one key" $
    A.head (SM.toUnfoldable obj)
  connectionUri ← note "Could not extract connectionUri" $
    J.toString =<< SM.lookup "connectionUri" =<< J.toObject inner
  pure { mountType, connectionUri }
