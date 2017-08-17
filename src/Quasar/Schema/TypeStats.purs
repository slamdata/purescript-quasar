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

import Control.Monad.Error.Class (throwError)
import Data.Argonaut as J
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record (record)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Generic (class Generic, gShow)
import Data.HugeInt (HugeInt)
import Data.HugeInt as HI
import Data.HugeNum (HugeNum)
import Data.HugeNum as HN
import Data.List as List
import Data.Maybe (Maybe, maybe)
import Data.StrMap as StrMap
import Data.String as Str
import Data.Tuple (Tuple(..))

type BoolStats =
  { true ∷ Number
  , false ∷ Number
  }

type CharStats =
  { count ∷ Number
  , min ∷ Char
  , max ∷ Char
  }

type StringStats =
  { count ∷ Number
  , minLength ∷ Number
  , maxLength ∷ Number
  , min ∷ String
  , max ∷ String
  }

type IntStats =
  { count ∷ Number
  , distribution ∷ Distribution
  , min ∷ HugeInt
  , max ∷ HugeInt
  }

type DecimalStats =
  { count ∷ Number
  , distribution ∷ Distribution
  , min ∷ HugeNum
  , max ∷ HugeNum
  }

type CollectionStats =
  { count ∷ Number
  , minLength ∷ Maybe Number
  , maxLength ∷ Maybe Number
  }

type Distribution =
  { mean ∷ Number
  , variance ∷ Number
  , skewness ∷ Number
  , kurtosis ∷ Number
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

-- TODO: use generics-rep when the nested record problem is fixed
derive instance genericTypeStats ∷ Generic TypeStats

instance showTypeStats ∷ Show TypeStats where
  show = gShow

encode ∷ TypeStats → J.Json
encode = CA.encode codec

decode ∷ J.Json → Either String TypeStats
decode = lmap CA.printJsonDecodeError <<< CA.decode codec

codec ∷ CA.JsonCodec TypeStats
codec = C.basicCodec dec enc
  where
  dec json = do
    obj ← C.decode CA.jobject json
    tag ← C.decode (CA.prop "kind" CA.string) obj
    case tag of
      "bool" → Bool <$> C.decode boolCodec obj
      "char" → Char <$> C.decode charCodec obj
      "string" → String <$> C.decode stringCodec obj
      "integer" → Int <$> C.decode intCodec obj
      "decimal" → Decimal <$> C.decode decimalCodec obj
      "collection" → Collection <$> C.decode collectionCodec obj
      "count" → Count <<< _.count <$> C.decode countCodec obj
      _ → throwError (CA.UnexpectedValue json)

  enc = case _ of
    Bool st → kinded "bool" (C.encode boolCodec st)
    Char st → kinded "char" (C.encode charCodec st)
    String st → kinded "string" (C.encode stringCodec st)
    Int st → kinded "integer" (C.encode intCodec st)
    Decimal st → kinded "decimal" (C.encode decimalCodec st)
    Collection st → kinded "collection" (C.encode collectionCodec st)
    Count count → kinded "count" (C.encode countCodec { count })

  kinded str =
    List.Cons (Tuple "kind" (J.fromString str))
      >>> StrMap.fromFoldable
      >>> J.fromObject

boolCodec ∷ CA.JPropCodec BoolStats
boolCodec = record
  { true: CA.number
  , false: CA.number
  }

charCodec ∷ CA.JPropCodec CharStats
charCodec = record
  { count: CA.number
  , min: CA.char
  , max: CA.char
  }

stringCodec ∷ CA.JPropCodec StringStats
stringCodec = record
  { count: CA.number
  , minLength: CA.number
  , maxLength: CA.number
  , min: CA.string
  , max: CA.string
  }

intCodec ∷ CA.JPropCodec IntStats
intCodec = record
  { count: CA.number
  , distribution: distributionCodec
  , min: hugeIntCodec
  , max: hugeIntCodec
  }

decimalCodec ∷ CA.JPropCodec DecimalStats
decimalCodec = record
  { count: CA.number
  , distribution: distributionCodec
  , min: hugeNumCodec
  , max: hugeNumCodec
  }

collectionCodec ∷ CA.JPropCodec CollectionStats
collectionCodec = record
  { count: CA.number
  , minLength: optionalCodec CA.number
  , maxLength: optionalCodec CA.number
  }

countCodec ∷ CA.JPropCodec { count ∷ Number }
countCodec = record
  { count: CA.number
  }

distributionCodec ∷ CA.JsonCodec Distribution
distributionCodec = CA.object "Distribution" $ record
  { mean: CA.number
  , variance: CA.number
  , skewness: CA.number
  , kurtosis: CA.number
  }

hugeIntCodec ∷ CA.JsonCodec HugeInt
hugeIntCodec =
  C.basicCodec
    (C.decode CA.string
      >=> HI.fromString
      >>> maybe (throwError (CA.TypeMismatch "HugeInt")) pure)
    (HI.toHugeNum
      >>> HN.toString
      >>> Str.takeWhile (not eq '.')
      >>> J.fromString)

hugeNumCodec ∷ CA.JsonCodec HugeNum
hugeNumCodec =
  C.basicCodec
    (C.decode CA.string
      >=> HN.fromString
      >>> maybe (throwError (CA.TypeMismatch "HugeNum")) pure)
    (HN.toString >>> J.fromString)

optionalCodec ∷ ∀ a. CA.JsonCodec a → CA.JsonCodec (Maybe a)
optionalCodec c =
  C.basicCodec
    (map Array.head <$> C.decode (CA.array c))
    (foldMap pure >>> CA.encode (CA.array c))
