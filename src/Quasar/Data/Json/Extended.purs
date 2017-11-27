module Quasar.Data.Json.Extended
  ( resultsAsEJson
  , resultsAsEJson'
  , decodeEJsonQ
  , module Data.Json.Extended
  ) where

import Prelude

import Control.Monad.Eff.Exception as Exn
import Data.Argonaut (Json)
import Data.Bifunctor as BF
import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Json.Extended (EJson, decodeEJson)
import Data.Newtype (unwrap)
import Data.Traversable as TR
import Quasar.Error (QError(..))

resultsAsEJson
  ∷ ∀ f
  . TR.Traversable f
  ⇒ Either QError (f (Array Json))
  → Either QError (f (Array EJson))
resultsAsEJson = (TR.traverse (TR.traverse decodeEJsonQ) =<< _)

resultsAsEJson'
  ∷ Either QError (Array Json)
  → Either QError (Array EJson)
resultsAsEJson' = map Identity >>> resultsAsEJson >>> map unwrap

decodeEJsonQ ∷ Json → Either QError EJson
decodeEJsonQ = decodeEJson >>> BF.lmap (Exn.error >>> Error)
