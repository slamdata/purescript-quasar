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

module Quasar.Data.MediaTypes
  ( applicationLDJSON
  , applicationZip
  , zipped
  , module Data.MediaType.Common
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (over2)
import Data.String as S

applicationLDJSON ∷ MediaType
applicationLDJSON = MediaType "application/ldjson;mode=readable"

applicationZip ∷ MediaType
applicationZip = MediaType "application/zip"

zipped ∷ MediaType → MediaType
zipped = over2 MediaType go applicationZip
  where
  go z mt = case S.stripPrefix (S.Pattern z) mt of
    Nothing → z <> "," <> mt
    _ → mt
