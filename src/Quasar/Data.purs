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

module Quasar.Data
  ( QData(..)
  , module Exports
  ) where

import Prelude

import Data.Argonaut (JArray, fromArray)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..), snd)

import Network.HTTP.Affjax.Request (class Requestable, toRequest)

import Quasar.Data.CSVOptions (CSVOptions, defaultCSVOptions) as Exports
import Quasar.Data.CSVOptions as CSV
import Quasar.Data.JSONMode (JSONMode(..)) as Exports
import Quasar.Data.JSONMode as JSON

data QData
  = JSON JSON.JSONMode JArray
  | LDJSON JSON.JSONMode String
  | CSV CSV.CSVOptions String
  | CustomData MediaType String

instance requestableQData ∷ Requestable QData where
  toRequest (JSON mode jarr) =
    Tuple
      (Just (JSON.decorateMode applicationJSON mode))
      (snd (toRequest (show (fromArray jarr)))) -- `show` for Argonaut's Json is just `stringify`
  toRequest (LDJSON mode content) =
    Tuple
      (Just (JSON.decorateMode applicationLDJSON mode))
      (snd (toRequest content))
  toRequest (CSV mode content) =
    Tuple (Just (CSV.toMediaType mode)) (snd (toRequest content))
  toRequest (CustomData mediaType content) =
    Tuple (Just mediaType) (snd (toRequest content))

applicationLDJSON ∷ MediaType
applicationLDJSON = MediaType "application/ldjson;mode=readable"
