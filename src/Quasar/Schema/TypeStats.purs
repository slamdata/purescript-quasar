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

module Quasar.Schema.TypeStats where

import Prelude
import Data.HugeInt (HugeInt)
import Data.HugeNum (HugeNum)
import Data.Maybe (Maybe)

type BoolStats =
  { true :: Number
  , false :: Number
  }

type CharStats =
  { count :: Number
  , min :: Char
  , max :: Char
  }

type StringStats =
  { count :: Number
  , minLength :: Number
  , maxLength :: Number
  , min :: String
  , max :: String
  }

type IntStats =
  { count :: Number
  , distribution :: Distribution
  , min :: HugeInt
  , max :: HugeInt
  }

type DecimalStats =
  { count :: Number
  , distribution :: Distribution
  , min :: HugeNum
  , max :: HugeNum
  }

type CollectionStats =
  { count :: Number
  , minLength :: Maybe Number
  , maxLength :: Maybe Number
  }

type Distribution =
  { mean :: Number
  , variance :: Number
  , skewness :: Number
  , kurtosis :: Number
  }

data TypeStats
  = Bool BoolStats
  | Char CharStats
  | String StringStats
  | Int IntStats
  | Decimal DecimalStats
  | Collection CollectionStats
  | Count Number

derive instance eqTypeStats ∷ Eq TypeStats
derive instance ordTypeStats ∷ Ord TypeStats
