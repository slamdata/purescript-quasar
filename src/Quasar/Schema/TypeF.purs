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

module Quasar.Schema.TypeF where

import Prelude hiding (const, map, bottom, top)

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Prism', prism')
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, traverse)
import Matryoshka (class Corecursive, class Recursive, embed, project)
import Quasar.Schema.SimpleType (SimpleType)

data BigInt
data BigDecimal

data TypeF j a
  = Bottom
  | Top
  | Simple SimpleType
  | Const j
  | Array (List a)
  | Map (Map j a)
  | Union (NonEmptyList a)

derive instance eqTypeF ∷ (Eq j, Eq a) ⇒ Eq (TypeF j a)
instance eq1TypeF ∷ Eq j ⇒ Eq1 (TypeF j) where eq1 = eq

derive instance ordTypeF ∷ (Ord j, Ord a) ⇒ Ord (TypeF j a)
instance ord1TypeF ∷ Ord j ⇒ Ord1 (TypeF j) where compare1 = compare

derive instance genericTypeF ∷ Generic (TypeF j a) _

instance showTypeF ∷ (Show j, Show a) ⇒ Show (TypeF j a) where
  show = genericShow

instance functorTypeF ∷ Functor (TypeF j) where
  map f = case _ of
    Bottom → Bottom
    Top → Top
    Simple ty → Simple ty
    Const j → Const j
    Array as → Array (f <$> as)
    Map ks → Map (f <$> ks)
    Union as → Union (f <$> as)

instance foldableTypeF ∷ Foldable (TypeF j) where
  foldl f b = case _ of
    Bottom → b
    Top → b
    Simple _ → b
    Const _ → b
    Array as → foldl f b as
    Map ks → foldl f b ks
    Union as → foldl f b as
  foldr f b = case _ of
    Bottom → b
    Top → b
    Simple _ → b
    Const _ → b
    Array as → foldr f b as
    Map ks → foldr f b ks
    Union as → foldr f b as
  foldMap f = case _ of
    Bottom → mempty
    Top → mempty
    Simple _ → mempty
    Const _ → mempty
    Array as → foldMap f as
    Map ks → foldMap f ks
    Union as → foldMap f as

instance traversableTypeF ∷ Traversable (TypeF j) where
  traverse f = case _ of
    Bottom → pure Bottom
    Top → pure Top
    Simple ty → pure (Simple ty)
    Const j → pure (Const j)
    Array as → Array <$> traverse f as
    Map ks → Map <$> traverse f ks
    Union as → Union <$> traverse f as
  sequence = traverse id

bottom ∷ ∀ j t. Corecursive t (TypeF j) ⇒ t
bottom = embed Bottom

top ∷ ∀ j t. Corecursive t (TypeF j) ⇒ t
top = embed Top

simple ∷ ∀ j t. Corecursive t (TypeF j) ⇒ SimpleType → t
simple = embed <<< Simple

const ∷ ∀ j t. Corecursive t (TypeF j) ⇒ j → t
const = embed <<< Const

array ∷ ∀ j t. Corecursive t (TypeF j) ⇒ List t → t
array = embed <<< Array

map ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Map j t → t
map = embed <<< Map

union ∷ ∀ j t. Corecursive t (TypeF j) ⇒ NonEmptyList t → t
union = embed <<< Union

_Bottom ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t Unit
_Bottom = prism' (\_ -> bottom) $ project >>> case _ of
  Bottom → Just unit
  _ → Nothing

_Top ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t Unit
_Top = prism' (\_ -> top) $ project >>> case _ of
  Top → Just unit
  _ → Nothing

_Simple ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t SimpleType
_Simple = prism' simple $ project >>> case _ of
  Simple ty → Just ty
  _ → Nothing

_Const ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t j
_Const = prism' const $ project >>> case _ of
  Const j → Just j
  _ → Nothing

_Array ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t (List t)
_Array = prism' array $ project >>> case _ of
  Array as → Just as
  _ → Nothing

_Map ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t (Map j t)
_Map = prism' map $ project >>> case _ of
  Map kvs → Just kvs
  _ → Nothing

_Union ∷ ∀ j t. Corecursive t (TypeF j) ⇒ Recursive t (TypeF j) ⇒ Prism' t (List t)
_Union = prism' array $ project >>> case _ of
  Array as → Just as
  _ → Nothing
