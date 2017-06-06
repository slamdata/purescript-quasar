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

module Quasar.QuasarF.Interpreter.Aff
  ( eval
  , module Quasar.QuasarF.Interpreter.Config
  ) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Functor.Coproduct (coproduct)

import Network.HTTP.Affjax as AX
import Network.HTTP.AffjaxF as AXF

import Quasar.ConfigF as CF
import Quasar.QuasarF (QuasarF)
import Quasar.QuasarF.Interpreter.Affjax as IAX
import Quasar.QuasarF.Interpreter.Config (Config)

eval
  ∷ ∀ m eff r
  . MonadReader { basePath ∷ AX.URL | r } m
  ⇒ MonadAff (ajax ∷ AX.AJAX | eff) m
  ⇒ MonadRec m
  ⇒ QuasarF
  ~> m
eval = foldFree (coproduct CF.evalReader (liftAff <<< AXF.eval)) <<< IAX.eval
