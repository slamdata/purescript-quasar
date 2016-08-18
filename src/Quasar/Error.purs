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

module Quasar.Error where

import Prelude

import Data.Either (Either)
import Control.Monad.Eff.Exception (Error, error, message)

data QError
  = NotFound
  | Unauthorized
  | Forbidden
  | PaymentRequired
  | Error Error

instance showQError ∷ Show QError where
  show NotFound = "NotFound"
  show Unauthorized = "Unauthorized"
  show Forbidden = "Forbidden"
  show PaymentRequired = "PaymentRequired"
  show (Error err) = "(Error " <> show err <> ")"

printQError ∷ QError → String
printQError = case _ of
  NotFound → "Resource not found"
  Unauthorized → "Resource is unavailable, authorization is required"
  Forbidden → "Resource is unavailable, the current authorization credentials do not grant access to the resource"
  PaymentRequired → "Resource is unavailable, payment is required to use this feature"
  Error err → message err

lowerQError ∷ QError → Error
lowerQError = case _ of
  Error err → err
  qe → error (printQError qe)

type QResponse resp = Either QError resp
type QContinuation resp next = QResponse resp → next

infixr 2 type QContinuation as :~>
