module Quasar.Data.Json.Extended
  ( resultsAsEJson
  , module Data.Json.Extended
  ) where

import Prelude

import Control.Monad.Eff.Exception as Exn

import Data.Argonaut (Json)
import Data.Bifunctor as BF
import Data.Either (Either)
import Data.Json.Extended (EJson, decodeEJson)
import Data.Traversable as TR

import Quasar.Error (QError(..))

resultsAsEJson
  ∷ Either QError (Array Json)
  → Either QError (Array EJson)
resultsAsEJson =
  flip bind $ TR.traverse $
    decodeEJson >>> BF.lmap (Exn.error >>> Error)
