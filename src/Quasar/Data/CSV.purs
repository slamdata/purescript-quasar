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

module Quasar.Data.CSV where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.MediaType (MediaType(..))
import Data.Newtype (class Newtype)
import Data.String as String

type OptionsR =
  { columnDelimiter ∷ String
  , rowDelimiter ∷ String
  , quoteChar ∷ String
  , escapeChar ∷ String
  , disposition ∷ String
  }

newtype Options = Options OptionsR

derive instance eqOptions ∷ Eq Options
derive instance ordOptions ∷ Ord Options
derive instance newtypeOptions ∷ Newtype Options _
derive instance genericOptions ∷ Generic Options _

defaultOptions ∷ Options
defaultOptions =
  Options
    { columnDelimiter: ","
    , rowDelimiter: "\n"
    , quoteChar: ""
    , escapeChar: "",
      disposition: "attachment"
    }

toMediaType ∷ Options → MediaType
toMediaType (Options opts) =
  MediaType $
    String.joinWith ";"
      [ "application/zip,text/csv"
      , "columnDelimiter=" <> opts.columnDelimiter
      , "rowDelimiter=" <> opts.rowDelimiter
      , "quoteChar=" <> opts.quoteChar
      , "escapeChar=" <> opts.escapeChar
      , "disposition=" <> opts.disposition
      ]
