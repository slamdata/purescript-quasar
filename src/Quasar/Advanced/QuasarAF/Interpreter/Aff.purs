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

module Quasar.Advanced.QuasarAF.Interpreter.Aff
  ( eval
  , module Quasar.Advanced.QuasarAF.Interpreter.Config
  ) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Functor.Coproduct (coproduct)
import Data.Maybe (Maybe)
import Data.NaturalTransformation (Natural)

import Network.HTTP.Affjax as AX
import Network.HTTP.AffjaxF as AXF

import OIDCCryptUtils.Types (IdToken)

import Quasar.Advanced.Auth (PermissionToken)
import Quasar.Advanced.QuasarAF (QuasarAFP)
import Quasar.Advanced.QuasarAF.Interpreter.Affjax as IAX
import Quasar.Advanced.QuasarAF.Interpreter.Config (Config)
import Quasar.ConfigF as CF

eval
  ∷ ∀ m eff r
  . ( MonadReader
        { basePath ∷ AX.URL
        , idToken ∷ Maybe IdToken
        , permissions ∷ Array PermissionToken
        | r
        }
        m
    , MonadAff (ajax ∷ AX.AJAX | eff) m
    , MonadRec m
    )
  ⇒ Natural QuasarAFP m
eval = foldFree (coproduct CF.evalReader (liftAff <<< AXF.eval)) <<< IAX.eval
