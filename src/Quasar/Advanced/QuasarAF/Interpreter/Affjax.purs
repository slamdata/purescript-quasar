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

module Quasar.Advanced.QuasarAF.Interpreter.Affjax
  ( M
  , eval
  , module Quasar.Advanced.QuasarAF.Interpreter.Config
  ) where

import Prelude

import Control.Bind ((<=<))
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Eff.Exception (Error, error)

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct, left, right, coproduct)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural)
import Data.Path.Pathy (printPath)
import Data.String as Str
import Data.Traversable (traverse)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req

import OIDCCryptUtils.Types as OIDC

import Quasar.Advanced.PermissionToken as PermissionToken
import Quasar.Advanced.OIDC as Provider
import Quasar.Advanced.Paths as Paths
import Quasar.Advanced.QuasarAF (QuasarAFP, QuasarAF(..))
import Quasar.Advanced.QuasarAF.Interpreter.Config (Config)
import Quasar.ConfigF as CF
import Quasar.QuasarF.Interpreter.Affjax as QCI
import Quasar.QuasarF.Interpreter.Internal (ask, mkRequest, jsonResult, defaultRequest)

type M r = Free (Coproduct (CF.ConfigF (Config r)) (AXF.AffjaxFP RequestContent String))

eval ∷ ∀ r. Natural QuasarAFP (M r)
eval = coproduct (evalQuasarCommunity <<< QCI.eval) evalQuasarAdvanced

evalQuasarCommunity
  ∷ ∀ r
  . Natural
      (Free (Coproduct (CF.ConfigF (Config r)) (AXF.AffjaxFP RequestContent String)))
      (M r)
evalQuasarCommunity = foldFree (coproduct (liftF <<< left) authify)
  where
  authify ∷ Natural (AXF.AffjaxFP RequestContent String) (M r)
  authify (AXF.AffjaxFP req k) = do
    { idToken, permissions } ← ask
    liftF $ right (AXF.AffjaxFP (insertAuthHeaders idToken permissions req) k)

evalQuasarAdvanced ∷ ∀ r. Natural QuasarAF (M r)
evalQuasarAdvanced (AuthProviders k) = do
  config ← ask
  k <$> mkRequest providerResult
    (AXF.affjax
       $ insertAuthHeaders
           config.idToken
           config.permissions
       $ defaultRequest
          { url = config.basePath <> Str.drop 1 (printPath Paths.oidcProviders) } )
  where
  providerResult ∷ String → Either Error (Array Provider.Provider)
  providerResult = lmap error <$> traverse Provider.fromJSON <=< jsonResult
evalQuasarAdvanced (ListTokens k) = hole
evalQuasarAdvanced (GetToken pid k) = hole
evalQuasarAdvanced (NewToken pid k) = hole
evalQuasarAdvanced (DeleteToken pid k) = hole
evalQuasarAdvanced (GroupInfo gr k) = hole
evalQuasarAdvanced (DeleteGroup gr k) = hole
evalQuasarAdvanced (ModifyGroup gr req k) = hole
evalQuasarAdvanced (Share with perms k) = hole

hole ∷ ∀ a. a
hole = Unsafe.Coerce.unsafeCoerce unit

insertAuthHeaders
  ∷ ∀ a
  . Maybe OIDC.IdToken
  → Array PermissionToken.PermissionToken
  → AX.AffjaxRequest a
  → AX.AffjaxRequest a
insertAuthHeaders idToken ps req =
  req
    { headers =
        req.headers
          <> (maybe [] (pure <<< authHeader) idToken)
          <> (maybe [] pure $ permissionsHeader ps)
    }

authHeader ∷ OIDC.IdToken → Req.RequestHeader
authHeader (OIDC.IdToken tok) =
  Req.RequestHeader "Authorization" ("Bearer " <> tok)

permissionsHeader :: Array PermissionToken.PermissionToken → Maybe Req.RequestHeader
permissionsHeader [] = Nothing
permissionsHeader ps =
  Just $
    Req.RequestHeader
      "X-Extra-PermissionTokens"
      (Str.joinWith "," (map PermissionToken.runPermissionToken ps))
