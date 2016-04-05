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

module Quasar.Data.QData
  ( QData
  , json
  , ldjson
  , csv
  , module Quasar.Data.CSV
  , module Quasar.Data.JSON
  , module Quasar.Data.JSONMode
  , module Quasar.Data.LDJSON
  ) where

import Prelude

import Data.Argonaut (fromArray)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Tuple (Tuple(..), snd)

import Network.HTTP.Affjax.Request (class Requestable, RequestContent, toRequest)

import Quasar.Data.CSV (CSV(..))
import Quasar.Data.CSV as CSV
import Quasar.Data.JSON (JSON(..))
import Quasar.Data.JSONMode (JSONMode(..))
import Quasar.Data.LDJSON (LDJSON(..))

newtype QData = QData (Tuple (Maybe MediaType) RequestContent)

qdata ∷ MediaType → RequestContent → QData
qdata mt = QData <<< Tuple (Just mt)

instance requestableQData ∷ Requestable QData where
  toRequest (QData req) = req

json ∷ JSON → QData
json (JSON mode jarr) =
  -- `show` for Argonaut's Json is just `stringify`
  qdata (Quasar.Data.JSON.toMediaType mode) $ snd (toRequest (show (fromArray jarr)))

ldjson ∷ LDJSON → QData
ldjson (LDJSON mode content) =
  qdata (Quasar.Data.LDJSON.toMediaType mode) $ snd (toRequest content)

csv ∷ CSV → QData
csv (CSV mode content) =
  qdata (CSV.toMediaType mode) (snd (toRequest content))
