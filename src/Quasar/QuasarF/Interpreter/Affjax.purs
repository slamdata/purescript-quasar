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

module Quasar.QuasarF.Interpreter.Affjax
  ( eval
  , module Quasar.QuasarF.Interpreter.Config
  )
  where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Free (Free)

import Data.Array (catMaybes)
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (applicationJSON)
import Data.NaturalTransformation (Natural)
import Data.Path.Pathy (printPath, runFileName, runDirName, rootDir, peel)
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd)

import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req

import Quasar.ConfigF as CF
import Quasar.Paths as Paths
import Quasar.QuasarF (QuasarF(..))
import Quasar.QuasarF.Interpreter.Config (Config)
import Quasar.QuasarF.Interpreter.Internal (mkURL, delete, unitResult, mkRequest, defaultRequest, get, jsonResult, put, toPageParams, strResult, toVarParams, ask)

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