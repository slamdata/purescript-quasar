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

module Quasar.Schema.SimpleType where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data SimpleType
  = Null
  | Bool
  | Byte
  | Char
  | Integer
  | Decimal

derive instance eqSimpleType ∷ Eq SimpleType
derive instance ordSimpleType ∷ Ord SimpleType
derive instance genericSimpleType ∷ Generic SimpleType _

instance showSimpleType ∷ Show SimpleType where
  show = genericShow
