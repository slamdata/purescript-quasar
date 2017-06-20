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

module Quasar.Advanced.QuasarAF.Interpreter.Affjax
  ( M
  , eval
  , authHeader
  , permissionsHeader
  , module Quasar.Advanced.QuasarAF.Interpreter.Config
  ) where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Eff.Exception (Error, error)

import Data.Argonaut (encodeJson, (:=), (~>), jsonEmptyObject)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Functor.Coproduct (Coproduct, left, right, coproduct)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy as Pt
import Data.String as Str
import Data.Tuple (snd)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req

import OIDC.Crypt.Types as OIDC

import Quasar.Advanced.Paths as Paths
import Quasar.Advanced.QuasarAF (QuasarAFC, QuasarAF(..))
import Quasar.Advanced.QuasarAF.Interpreter.Config (Config)
import Quasar.Advanced.Types as Qa
import Quasar.ConfigF as CF
import Quasar.Error (QResponse)
import Quasar.QuasarF.Interpreter.Affjax as QCI
import Quasar.QuasarF.Interpreter.Internal (ask, mkRequest, jsonResult, defaultRequest, unitResult)

type M r = Free (Coproduct (CF.ConfigF (Config r)) (AXF.AffjaxFP RequestContent String))

eval ∷ ∀ r. QuasarAFC ~> M r
eval = coproduct (evalQuasarCommunity <<< QCI.eval) evalQuasarAdvanced

evalQuasarCommunity
  ∷ ∀ r
  . Free (Coproduct (CF.ConfigF (Config r)) (AXF.AffjaxFP RequestContent String))
  ~> M r
evalQuasarCommunity = foldFree (coproduct (liftF <<< left) authify)
  where
  authify ∷ AXF.AffjaxFP RequestContent String ~> M r
  authify (AXF.AffjaxFP req k) = do
    { idToken, permissions } ← ask
    liftF $ right (AXF.AffjaxFP (insertAuthHeaders idToken permissions req) k)

evalQuasarAdvanced ∷ ∀ r. QuasarAF ~> M r
evalQuasarAdvanced (GroupInfo pt k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map Qa.runGroupInfo)
    $ _{ url =
            config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.group)
             <> Qa.printGroupPath pt
       }
evalQuasarAdvanced (CreateGroup pt k) = do
  config ← ask
  map k
    -- Note, I'm not sure that this response is empty
    $ mkAuthedRequest unitResult
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.group)
             <> Qa.printGroupPath pt
       , method = Left POST
       }
evalQuasarAdvanced (ModifyGroup pt patch k) = do
  config ← ask
  map k
    -- same as L#86
    $ mkAuthedRequest unitResult
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.group)
             <> Qa.printGroupPath pt
       , method = Left PATCH
       , content = Just $ snd $ toRequest $ encodeJson $ Qa.GroupPatch patch
       }
evalQuasarAdvanced (DeleteGroup pt k) = do
  config ← ask
  map k
    $ mkAuthedRequest unitResult
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.group)
             <> Qa.printGroupPath pt
       , method = Left DELETE
       }
evalQuasarAdvanced (PermissionList isTransitive k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map (map Qa.runPermission))
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.permission)
             <> (if isTransitive then "?transitive" else "")
       }
evalQuasarAdvanced (AuthorityList k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map (map Qa.runPermission))
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.authority)
       }
evalQuasarAdvanced (PermissionInfo pid k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map Qa.runPermission)
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.permission)
             <> Qa.runPermissionId pid
       }
evalQuasarAdvanced (PermissionChildren pid isTransitive k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map (map Qa.runPermission))
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.permission)
             <> Qa.runPermissionId pid
             <> "/children"
             <> (if isTransitive then "?transitive" else "")
       }
evalQuasarAdvanced (SharePermission req k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map (map Qa.runPermission))
    $ _{ url = config.basePath <> (Str.drop 1 $ Pt.printPath Paths.permission)
       , method = Left POST
       , content = Just $ snd $ toRequest $ encodeJson $ Qa.ShareRequest req
       }
evalQuasarAdvanced (DeletePermission pid k) = do
  config ← ask
  map k
    $ mkAuthedRequest unitResult
    $ _{ url =
           config.basePath
             <> (Str.drop 1 $ Pt.printPath Paths.permission)
             <> Qa.runPermissionId pid
       , method = Left DELETE
       }
evalQuasarAdvanced (TokenList k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map (map Qa.runToken))
    $ _{ url = config.basePath <> (Str.drop 1 $ Pt.printPath Paths.token) }
evalQuasarAdvanced (TokenInfo tid k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map Qa.runToken)
    $ _{ url = config.basePath <> (Str.drop 1 $ Pt.printPath Paths.token) <> Qa.runTokenId tid }
evalQuasarAdvanced (CreateToken mbName actions k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map Qa.runToken)
    $ _{ url = config.basePath <> (Str.drop 1 $ Pt.printPath Paths.token)
       , method = Left POST
       , content =
           Just $ snd $ toRequest
             $ "name" := maybe "" Qa.runTokenName mbName
             ~> "actions" := (map Qa.Action actions)
             ~> jsonEmptyObject
       }
evalQuasarAdvanced (DeleteToken tid k) = do
  config ← ask
  map k
    $ mkAuthedRequest unitResult
    $ _{ url = config.basePath <> (Str.drop 1 $ Pt.printPath Paths.token) <> Qa.runTokenId tid
       , method = Left DELETE
       }
evalQuasarAdvanced (AuthProviders k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >>> map (map Qa.runProvider))
    $ _{ url = config.basePath <> Str.drop 1 (Pt.printPath Paths.oidcProviders) }
evalQuasarAdvanced (Licensee k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >=> map (lmap error) Qa.decodeLicensee)
    $ _{ url = config.basePath <> "/server/licensee" }
evalQuasarAdvanced (LicenseInfo k) = do
  config ← ask
  map k
    $ mkAuthedRequest (jsonResult >=> map (lmap error) Qa.decodeLicenseInfo)
    $ _{ url = config.basePath <> "/server/licenseInfo" }


mkAuthedRequest
  ∷ ∀ a r
  . (String → Either Error a)
  → (AX.AffjaxRequest RequestContent → AX.AffjaxRequest RequestContent)
  → M r (QResponse a)
mkAuthedRequest handleResult updateRequest = do
  config ← ask
  mkRequest handleResult
    $ AXF.affjax
    $ insertAuthHeaders
        config.idToken
        config.permissions
    $ updateRequest
    $ defaultRequest

insertAuthHeaders
  ∷ ∀ a
  . Maybe OIDC.IdToken
  → Array Qa.TokenHash
  → AX.AffjaxRequest a
  → AX.AffjaxRequest a
insertAuthHeaders idToken hashes req =
  req
    { headers =
        req.headers
          <> (foldMap (pure <<< authHeader) idToken)
          <> (foldMap pure $ permissionsHeader hashes)
    }

authHeader ∷ OIDC.IdToken → Req.RequestHeader
authHeader (OIDC.IdToken tok) =
  Req.RequestHeader "Authorization" ("Bearer " <> tok)

permissionsHeader ∷ Array Qa.TokenHash → Maybe Req.RequestHeader
permissionsHeader [] = Nothing
permissionsHeader hs =
  Just $
    Req.RequestHeader
      "X-Extra-Permissions"
      (Str.joinWith "," (map Qa.runTokenHash hs))
