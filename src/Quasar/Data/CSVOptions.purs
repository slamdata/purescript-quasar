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

module Quasar.Data.CSVOptions where

import Prelude

import Data.MediaType (MediaType(..))
import Data.String as String

type CSVOptions =
  { columnDelimiter ∷ String
  , rowDelimiter ∷ String
  , quoteChar ∷ String
  , escapeChar ∷ String
  }

defaultCSVOptions ∷ CSVOptions
defaultCSVOptions =
  { columnDelimiter: ","
  , rowDelimiter: "\\n"
  , quoteChar: "\""
  , escapeChar: "\""
  }

toMediaType ∷ CSVOptions → MediaType
toMediaType opts = MediaType $
  String.joinWith ";"
    [ "text/csv"
    , "columnDelimiter=" <> opts.columnDelimiter
    , "rowDelimiter=" <> opts.rowDelimiter
    , "quoteChar=" <> opts.quoteChar
    , "escapeChar=" <> opts.escapeChar
    ]
