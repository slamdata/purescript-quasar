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

module Quasar.Error where

import Prelude

import Control.Monad.Eff.Exception (Error, error, message)
import Data.Argonaut (JObject)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)

newtype UnauthorizedDetails = UnauthorizedDetails String

data QError
  = NotFound
  | Unauthorized (Maybe UnauthorizedDetails)
  | Forbidden
  | PaymentRequired
  | ErrorMessage {title ∷ Maybe String, message ∷ String, raw ∷ JObject}
  | Error Error

type QErrors = NonEmptyList QError

instance showQError ∷ Show QError where
  show NotFound = "NotFound"
  show (Unauthorized Nothing) = "Unauthorized"
  show (Unauthorized (Just (UnauthorizedDetails details))) = "Unauthorized: " <> details
  show Forbidden = "Forbidden"
  show PaymentRequired = "PaymentRequired"
  show (ErrorMessage {title, message}) = "(ErrorMesssage {title: " <> show title <> ", message: " <> show message <> "})"
  show (Error err) = "(Error " <> show err <> ")"

printQError ∷ QError → String
printQError = case _ of
  NotFound → "Resource not found"
  Unauthorized _ → "Resource is unavailable, authorization is required"
  Forbidden → "Resource is unavailable, the current authorization credentials do not grant access to the resource"
  PaymentRequired → "Resource is unavailable, payment is required to use this feature"
  ErrorMessage {title, message} → maybe "" (_ <> ": ") title <> message
  Error err → message err

lowerQError ∷ QError → Error
lowerQError = case _ of
  Error err → err
  qe → error (printQError qe)

lowerQErrors ∷ NonEmptyList QError → Error
lowerQErrors errors =
  error
    $ "Multiple errors occured: "
    <> intercalate
         ", "
         (map printQError $ (NEL.toUnfoldable errors) :: Array QError)

type QResponse resp = Either (NonEmptyList QError) resp
type QContinuation resp next = QResponse resp → next

infixr 2 type QContinuation as :~>
