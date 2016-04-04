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

module Quasar.QuasarF.Interpreter.Aff (Config, eval) where

import Prelude

import Control.Bind ((=<<), (<=<))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Reader.Class (class MonadReader, ask)

import Data.Argonaut ((.?))
import Data.Argonaut as Json
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.NaturalTransformation (Natural)
import Data.Path.Pathy (Path, Rel, Abs, RelDir, Sandboxed, rootDir, file, dir, relativeTo, printPath, peel, runDirName, runFileName, (</>))
import Data.String as Str
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)

import Global (encodeURIComponent)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request as AXR
import Network.HTTP.RequestHeader as Req
import Network.HTTP.StatusCode (StatusCode(..))

import Quasar.PathsP as Paths
import Quasar.QuasarF (QuasarF(..), runLDJSON, AnyPath, Content, Pagination)

type Config = { basePath ∷ AX.URL }

eval
  ∷ ∀ m eff
  . ( MonadReader Config m
    , MonadAff (ajax ∷ AX.AJAX | eff) m
    )
  ⇒ Natural QuasarF m
eval = \q -> case q of

  ServerInfo k -> do
    { basePath } ← ask
    k <$> mkRequest jsonResult (AX.get (basePath <> Str.drop 1 (printPath Paths.serverInfo)))

  GetMetadata path k ->
    k <$> (mkRequest jsonResult <<< AX.get =<< mkURL Paths.metadata path Nil)

  ReadQuery path sql vars pagination k -> do
    let params = Tuple "q" sql : toVarParams vars <> toPageParams pagination
    url ← mkURL Paths.query path params
    k <$> mkRequest jarrResult (AX.get url)

  WriteQuery path file sql vars k -> do
    url ← mkURL Paths.query path (toVarParams vars)
    k <$> mkRequest jsonResult
      (AX.affjax $ AX.defaultRequest
        { url = url
        , method = Left POST
        , headers = [Req.RequestHeader "Destination" (printPath file)]
        , content = Just sql
        })

  CompileQuery path sql vars k -> do
    url ← mkURL Paths.compile path (Tuple "q" sql : toVarParams vars)
    k <$> mkRequest strResult (AX.get url)

  ReadFile path pagination k -> do
    url ← mkURL Paths.data_ (Right path) (toPageParams pagination)
    k <$> mkRequest jarrResult
      (AX.affjax AX.defaultRequest
        { url = url
        , headers = [Req.Accept applicationJSON]
        })

  WriteFile path content k -> do
    url ← mkURL Paths.data_ (Right path) Nil
    k <$> mkRequest unitResult
      (AX.affjax (mkDataAReq content) { url = url, method = Left PUT })

  AppendFile path content k -> do
    url ← mkURL Paths.data_ (Right path) Nil
    k <$> mkRequest unitResult
      (AX.affjax (mkDataAReq content) { url = url, method = Left POST })

  DeleteFile file k -> do
    url ← mkURL Paths.data_ (Right file) Nil
    k <$> mkRequest unitResult (AX.delete url)

  MoveFile fromPath toPath k -> do
    url ← mkURL Paths.data_ (Right fromPath) Nil
    k <$> mkRequest unitResult
      (AX.affjax AX.defaultRequest
        { url = url
        , method = Left MOVE
        , headers = [Req.RequestHeader "Destination" (printPath toPath)]
        })

  CreateMount path config k -> do
    let pathParts = either peel peel path
        parentDir = maybe rootDir fst pathParts
        name = maybe "" (either runDirName runFileName <<< snd) pathParts
    url ← mkURL Paths.mount (Left parentDir) Nil
    k <$> mkRequest unitResult
      (AX.affjax AX.defaultRequest
        { url = url
        , method = Left POST
        , headers = [Req.RequestHeader "X-File-Name" name]
        , content = Just config
        })

  UpdateMount path config k -> do
    url ← mkURL Paths.mount path Nil
    k <$> mkRequest unitResult (AX.put url config)

  GetMount path k ->
    k <$> (mkRequest jsonResult <<< AX.get =<< mkURL Paths.mount path Nil)

  DeleteMount path k ->
    k <$> (mkRequest unitResult <<< AX.delete =<< mkURL Paths.mount path Nil)

  where

  jsonResult ∷ String → Either Error Json.Json
  jsonResult = lmap error <$> Json.jsonParser

  jarrResult ∷ String → Either Error Json.JArray
  jarrResult = lmap error <$> Json.decodeJson <=< jsonResult

  strResult ∷ String → Either Error String
  strResult = Right

  unitResult ∷ String → Either Error Unit
  unitResult = (const (Right unit))

  toVarParams ∷ SM.StrMap String → List (Tuple String String)
  toVarParams = map (lmap ("var." <> _)) <<< SM.toList

  toPageParams ∷ Maybe Pagination → List (Tuple String String)
  toPageParams Nothing = Nil
  toPageParams (Just { offset, limit })
    = Tuple "offset" (show offset)
    : Tuple "limit" (show limit)
    : Nil

mkURL
  ∷ ∀ m
  . (MonadReader Config m)
  ⇒ RelDir Sandboxed
  → AnyPath
  → List (Tuple String String)
  → m String
mkURL endpoint path params = do
  { basePath } ← ask
  let url = basePath <> mkPath endpoint path
  pure case params of
    Nil → url
    _ → url <> toQueryString params
  where
  toQueryString ∷ List (Tuple String String) → String
  toQueryString
    = ("?" <> _)
    <<< Str.joinWith "&"
    <<< List.toUnfoldable
    <<< map (\(Tuple k v) → k <> "=" <> encodeURIComponent v)

mkPath ∷ RelDir Sandboxed → AnyPath → String
mkPath base fsPath
  = Str.drop 1
  $ either printPath printPath
  $ bimap (baseify (dir "/")) (baseify (file "")) fsPath
  where
  baseify
    ∷ ∀ b. Path Rel b Sandboxed → Path Abs b Sandboxed → Path Rel b Sandboxed
  baseify x p = base </> fromMaybe x (p `relativeTo` rootDir)

mkDataAReq ∷ Content → AX.AffjaxRequest AXR.RequestContent
mkDataAReq content =
  AX.defaultRequest
    { headers = [Req.ContentType (ty content)]
    , content = Just $ snd (rc content)
    }
  where
  ty = either (const applicationLDJSON) (const applicationJSON)
  rc = either (AXR.toRequest <<< runLDJSON) (AXR.toRequest <<< Json.fromArray)
  applicationLDJSON = MediaType "application/ldjson"

mkRequest
  ∷ ∀ eff m a
  . (MonadAff eff m)
  ⇒ (String → Either Error a)
  → Aff eff (AX.AffjaxResponse String)
  → m (Either Error a)
mkRequest f aff = liftAff $ handleResult f <$> attempt aff

handleResult
  ∷ ∀ a
  . (String → Either Error a)
  → Either Error (AX.AffjaxResponse String)
  → Either Error a
handleResult f result =
  case result of
    Right { status, response }
      | succeeded status → f response
      | otherwise →
          Left $ error $
            either (pure $ "An unknown error ocurred: " ++ show response) id $
              (_ .? "error") =<< Json.decodeJson =<< Json.jsonParser response
    Left err → Left err
  where
  succeeded :: StatusCode -> Boolean
  succeeded (StatusCode code) = code >= 200 && code < 300
