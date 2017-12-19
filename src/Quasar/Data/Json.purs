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

module Quasar.Data.Json where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.MediaType (MediaType(..))
import Data.Newtype (class Newtype)
import Quasar.Data.MediaTypes (applicationJSON, applicationLDJSON)

type OptionsR =
  { encoding ∷ EncodingStyle
  , precision ∷ PrecisionMode
  }

newtype Options = Options OptionsR

derive instance eqOptions ∷ Eq Options
derive instance ordOptions ∷ Ord Options
derive instance newtypeOptions ∷ Newtype Options _
derive instance genericOptions ∷ Generic Options _

data PrecisionMode = Readable | Precise

derive instance eqPrecisionMode ∷ Eq PrecisionMode
derive instance ordPrecisionMode ∷ Ord PrecisionMode
derive instance genericPrecisionMode ∷ Generic PrecisionMode _

instance showPrecisionMode ∷ Show PrecisionMode where show = genericShow

decorateMode ∷ PrecisionMode → MediaType → MediaType
decorateMode mode (MediaType mt) = MediaType (mt <> modeToString mode)
  where
  modeToString Readable = ";mode=readable"
  modeToString Precise = ";mode=precise"

data EncodingStyle = Array | LineDelimited

derive instance eqEncodingStyle ∷ Eq EncodingStyle
derive instance ordEncodingStyle ∷ Ord EncodingStyle
derive instance genericEncodingStyle ∷ Generic EncodingStyle _

instance showEncodingStyle ∷ Show EncodingStyle where show = genericShow

toMediaType ∷ Options → MediaType
toMediaType (Options { precision, encoding }) =
  decorateMode precision
    case encoding of
      Array → applicationJSON
      LineDelimited → applicationLDJSON
