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

module Quasar.Data.LDJSON
  ( LDJSON(..)
  , toMediaType
  , module Quasar.Data.JSONMode
  ) where

import Data.MediaType (MediaType(..))

import Quasar.Data.JSONMode (JSONMode(..))

data LDJSON = LDJSON JSONMode String

toMediaType ∷ JSONMode → MediaType
toMediaType Readable = MediaType "application/ldjson;mode=readable"
toMediaType Precise = MediaType "application/ldjson;mode=precise"
