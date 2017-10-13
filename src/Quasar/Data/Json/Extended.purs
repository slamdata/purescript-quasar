module Quasar.Data.Json.Extended
  ( resultsAsEJson
  , module Data.Json.Extended
  ) where

import Prelude

import Control.Monad.Eff.Exception as Exn

import Data.Argonaut (Json)
import Data.Bifunctor as BF
import Data.Either (Either)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Json.Extended (EJson, decodeEJson)
import Data.Traversable as TR

import Quasar.Error (QError(..))

resultsAsEJson
  ∷ Either (NonEmptyList QError) (Array Json)
  → Either (NonEmptyList QError) (Array EJson)
resultsAsEJson =
  flip bind $ TR.traverse $
    decodeEJson >>> BF.lmap (Exn.error >>> Error >>> NEL.singleton)
