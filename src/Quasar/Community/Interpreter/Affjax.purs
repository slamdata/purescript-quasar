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

module Quasar.Community.Interpreter.Affjax
  ( eval
  , module Quasar.Community.Interpreter.Config
  ) where

import Prelude

import Control.Bind ((=<<), (<=<))
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Free (Free, liftF)

import Data.Array (catMaybes)
import Data.Argonaut ((.?))
import Data.Argonaut as Json
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Data.NaturalTransformation (Natural)
import Data.Path.Pathy (Path, Rel, Abs, RelDir, Sandboxed, rootDir, file, dir, relativeTo, printPath, peel, runDirName, runFileName, (</>))
import Data.String as Str
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)

import Global (encodeURIComponent)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req
import Network.HTTP.StatusCode (StatusCode(..))

import Quasar.Community.Interpreter.Config (Config)
import Quasar.Community.Paths as Paths
import Quasar.Community.QuasarF (QuasarF(..), QError(..), AnyPath, Pagination)
import Quasar.ConfigF as CF

type M = Free (Coproduct (CF.ConfigF Config) (AXF.AffjaxFP RequestContent String))

eval ∷ Natural QuasarF M
eval = \q -> case q of

  ServerInfo k -> do
    { basePath } ← ask
    k <$> mkRequest jsonResult (get (basePath <> Str.drop 1 (printPath Paths.serverInfo)))

  GetMetadata path k ->
    k <$> (mkRequest jsonResult <<< get =<< mkURL Paths.metadata path Nil)

  ReadQuery path sql vars pagination k -> do
    let params = Tuple "q" sql : toVarParams vars <> toPageParams pagination
    url ← mkURL Paths.query path params
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , headers = [Req.Accept applicationJSON]
        })

  WriteQuery path file sql vars k -> do
    url ← mkURL Paths.query path (toVarParams vars)
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , method = Left POST
        , headers = [Req.RequestHeader "Destination" (printPath file)]
        , content = Just $ snd (toRequest sql)
        })

  CompileQuery path sql vars k -> do
    url ← mkURL Paths.compile path (Tuple "q" sql : toVarParams vars)
    k <$> mkRequest strResult (get url)

  ReadFile path pagination k -> do
    url ← mkURL Paths.data_ (Right path) (toPageParams pagination)
    k <$> mkRequest jsonResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = [Req.Accept applicationJSON]
        })

  WriteFile path content k -> do
    url ← mkURL Paths.data_ (Right path) Nil
    let reqSettings = toRequest content
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = catMaybes [Req.ContentType <$> fst reqSettings]
        , method = Left PUT
        , content = Just $ snd reqSettings
        })

  AppendFile path content k -> do
    url ← mkURL Paths.data_ (Right path) Nil
    let reqSettings = toRequest content
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = catMaybes [Req.ContentType <$> fst reqSettings]
        , method = Left POST
        , content = Just $ snd (toRequest content)
        })

  DeleteData path k -> do
    k <$> (mkRequest unitResult <<< delete =<< mkURL Paths.data_ path Nil)

  MoveData fromPath toPath k -> do
    url ← mkURL Paths.data_ fromPath Nil
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , method = Left MOVE
        , headers = [Req.RequestHeader "Destination" (either printPath printPath toPath)]
        })

  CreateMount path config k -> do
    let pathParts = either peel peel path
        parentDir = maybe rootDir fst pathParts
        name = maybe "" (either runDirName runFileName <<< snd) pathParts
    url ← mkURL Paths.mount (Left parentDir) Nil
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , method = Left POST
        , headers = [Req.RequestHeader "X-File-Name" name]
        , content = Just $ snd (toRequest config)
        })

  UpdateMount path config k -> do
    url ← mkURL Paths.mount path Nil
    k <$> (mkRequest unitResult $ put url $ snd (toRequest config))

  GetMount path k ->
    k <$> (mkRequest jsonResult <<< get =<< mkURL Paths.mount path Nil)

  MoveMount fromPath toPath k -> do
    url ← mkURL Paths.mount fromPath Nil
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , method = Left MOVE
        , headers = [Req.RequestHeader "Destination" (either printPath printPath toPath)]
        })

  DeleteMount path k ->
    k <$> (mkRequest unitResult <<< delete =<< mkURL Paths.mount path Nil)

  where

  jsonResult ∷ ∀ j. Json.DecodeJson j ⇒ String → Either Error j
  jsonResult = lmap error <$> (Json.decodeJson <=< Json.jsonParser)

  strResult ∷ String → Either Error String
  strResult = Right

  unitResult ∷ String → Either Error Unit
  unitResult = const (Right unit)

  toVarParams ∷ SM.StrMap String → List (Tuple String String)
  toVarParams = map (lmap ("var." <> _)) <<< SM.toList

  toPageParams ∷ Maybe Pagination → List (Tuple String String)
  toPageParams Nothing = Nil
  toPageParams (Just { offset, limit })
    = Tuple "offset" (show offset)
    : Tuple "limit" (show limit)
    : Nil

  defaultRequest ∷ AX.AffjaxRequest RequestContent
  defaultRequest = AX.defaultRequest { content = Nothing }

  get :: AX.URL -> AXF.AffjaxF RequestContent String
  get u = AXF.affjax (defaultRequest { url = u })

  put :: AX.URL -> RequestContent -> AXF.AffjaxF RequestContent String
  put u c = AXF.affjax (defaultRequest { method = Left PUT, url = u, content = Just c })

  delete :: AX.URL -> AXF.AffjaxF RequestContent String
  delete u = AXF.affjax (defaultRequest { method = Left DELETE, url = u })

ask ∷ M Config
ask = liftF $ left $ CF.GetConfig id

mkURL
  ∷ RelDir Sandboxed
  → AnyPath
  → List (Tuple String String)
  → M String
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

mkRequest
  ∷ ∀ a
  . (String → Either Error a)
  → AXF.AffjaxF RequestContent String
  → M (Either QError a)
mkRequest f = map (handleResult f) <<< liftF <<< right

handleResult
  ∷ ∀ a
  . (String → Either Error a)
  → Either Error (AX.AffjaxResponse String)
  → Either QError a
handleResult f result =
  case result of
    Right { status: StatusCode code, response }
      | code >= 200 && code < 300 → lmap Error (f response)
      | code == 404 → Left NotFound
      | otherwise →
          Left $ Error $ error $
            either (pure $ "An unknown error ocurred: " ++ show response) id $
              (_ .? "error") =<< Json.decodeJson =<< Json.jsonParser response
    Left err → Left (Error err)
