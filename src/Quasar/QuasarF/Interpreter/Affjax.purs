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

module Quasar.QuasarF.Interpreter.Affjax
  ( M
  , eval
  , module Quasar.QuasarF.Interpreter.Config
  )
  where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Free (Free)
import Data.Argonaut (Json, JObject, jsonEmptyObject, (:=), (~>))
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldl, foldMap)
import Data.Functor.Coproduct (Coproduct)
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.List (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Monoid (mempty)
import Data.Path.Pathy (peel, printPath, rootDir, runDirName, runFileName)
import Data.StrMap as SM
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.URI as URI
import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.RequestHeader as Req
import Quasar.ConfigF as CF
import Quasar.Data.Json as Json
import Quasar.Data.MediaTypes (applicationZip)
import Quasar.FS.DirMetadata as DirMetadata
import Quasar.Metastore as Metastore
import Quasar.Mount as Mount
import Quasar.Paths as Paths
import Quasar.QuasarF (QuasarF(..), DirPath)
import Quasar.QuasarF.Interpreter.Config (Config)
import Quasar.QuasarF.Interpreter.Internal (defaultRequest, delete, get, jsonResult, mkFSUrl, mkRequest, mkUrl, put, strResult, toPageParams, toVarParams, unitResult, mkRequest', withExpired)
import Quasar.Query.OutputMeta as QueryOutputMeta
import Quasar.ServerInfo as ServerInfo
import Quasar.Types as QT
import SqlSquared as Sql

type M r = Free (Coproduct (CF.ConfigF (Config r)) (AXF.AffjaxFP RequestContent String))

eval ∷ ∀ r. QuasarF ~> M r
eval = case _ of
  ServerInfo k → do
    url ← mkUrl (Right Paths.serverInfo) mempty
    k <$> mkRequest serverInfoResult (get url)

  FileMetadata path k → do
    url ← mkFSUrl Paths.metadata (Right path) mempty
    k <$> mkRequest fileMetaResult (get url)

  DirMetadata path pagination k → do
    url ← mkFSUrl Paths.metadata (Left path) (toPageParams pagination)
    k <$> mkRequest (resourcesResult path) (get url)

  ReadQuery mode path sql vars pagination k → do
    let params = querySingleton "q" (Sql.printQuery sql) <> toVarParams vars <> toPageParams pagination
    url ← mkFSUrl Paths.query (Left path) params
    k <$> mkRequest jsonResult
      (AXF.affjax $ defaultRequest
        { url = url
        , headers = [Req.Accept $ Json.decorateMode mode applicationJSON]
        })

  WriteQuery path file sql vars k → do
    let destHeader = Tuple "Destination" (printPath file)
    url ← mkFSUrl Paths.query (Left path) (headerParams [destHeader] <> toVarParams vars)
    k <$> mkRequest writeQueryResult
      (AXF.affjax $ defaultRequest
        { url = url
        , method = Left POST
        , content = Just $ snd (toRequest $ Sql.printQuery sql)
        })

  CompileQuery path sql vars k → do
    url ← mkFSUrl Paths.compile (Left path) (querySingleton "q" (Sql.printQuery sql) <> toVarParams vars)
    k <$> mkRequest (lmap error <$> QT.compileResultFromString <=< strResult) (get url)

  ReadFile mode path pagination k → do
    url ← mkFSUrl Paths.data_ (Right path) (toPageParams pagination)
    eitherJarr ← mkRequest' jsonResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = [Req.Accept $ Json.decorateMode mode applicationJSON]
        })
    pure $ k $ map withExpired eitherJarr

  WriteFile path content k → do
    url ← mkFSUrl Paths.data_ (Right path) mempty
    let reqSettings = toRequest content
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = catMaybes [Req.ContentType <$> fst reqSettings]
        , method = Left PUT
        , content = Just $ snd reqSettings
        })

  WriteDir path content k → do
    url ← mkFSUrl Paths.data_ (Left path) mempty
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = [Req.ContentType applicationZip]
        , method = Left PUT
        , content = Just $ snd (toRequest content)
        })

  AppendFile path content k → do
    url ← mkFSUrl Paths.data_ (Right path) mempty
    let reqSettings = toRequest content
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , headers = catMaybes [Req.ContentType <$> fst reqSettings]
        , method = Left POST
        , content = Just $ snd (toRequest content)
        })

  InvokeFile mode path vars pagination k → do
    -- We can't use toVarParams here, as the format is different for invokeFile,
    -- instead of var.x=3 it's just x=3
    url ←
      mkFSUrl Paths.invoke
        (Right path)
        ( URI.Query
            (map Just <$> SM.toUnfoldable vars)
            <> toPageParams pagination)
    eitherJarr ←
      mkRequest' jsonResult
        (AXF.affjax defaultRequest
          { url = url
          , headers = [Req.Accept $ Json.decorateMode mode applicationJSON]
          })
    pure $ k $ map withExpired eitherJarr

  DeleteData path k → do
    k <$> (mkRequest unitResult <<< delete =<< mkFSUrl Paths.data_ path mempty)

  MoveData fromPath toPath k → do
    let destHeader = Tuple "Destination" (either printPath printPath toPath)
    url ← mkFSUrl Paths.data_ fromPath (headerParams [destHeader])
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , method = Left MOVE
        })

  CreateMount path config mbMaxAge k → do
    let pathParts = either peel peel path
        parentDir = maybe rootDir fst pathParts
        name = maybe "" (either runDirName runFileName <<< snd) pathParts
        filenameHeader = Tuple "X-File-Name" name
    url ← mkFSUrl Paths.mount (Left parentDir) (headerParams [filenameHeader])
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , method = Left POST
        , headers = foldMap (pure <<< maxAgeHeader) mbMaxAge
        , content = Just $ snd (toRequest (Mount.toJSON config))
        })

  UpdateMount path config mbMaxAge k → do
    url ← mkFSUrl Paths.mount path mempty
    k <$> mkRequest unitResult
      (AXF.affjax defaultRequest
        { url = url
        , method = Left PUT
        , headers = foldMap (pure <<< maxAgeHeader) mbMaxAge
        , content = Just $ snd (toRequest (Mount.toJSON config))
        })

  GetMount path k →
    k <$> (mkRequest mountConfigResult <<< get =<< mkFSUrl Paths.mount path mempty)

  GetMetastore k → do
    url ← mkUrl (Right Paths.metastore) mempty
    k <$> mkRequest metastoreResult (get url)

  PutMetastore { initialize, metastore } k → do
    let query = if initialize then URI.Query (singleton (Tuple "initialize" Nothing)) else mempty
    url ← mkUrl (Right Paths.metastore) query
    k <$> (mkRequest unitResult $ put url $ snd (toRequest (Metastore.toJSON metastore)))


serverInfoResult ∷ String -> Either Error ServerInfo.ServerInfo
serverInfoResult = lmap error <$> ServerInfo.fromJSON <=< jsonResult

writeQueryResult ∷ String → Either Error QueryOutputMeta.OutputMeta
writeQueryResult = lmap error <$> QueryOutputMeta.fromJSON <=< jsonResult

resourcesResult ∷ DirPath → String → Either Error DirMetadata.DirMetadata
resourcesResult path = lmap error <$> DirMetadata.fromJSON path <=< jsonResult

mountConfigResult ∷ String → Either Error Mount.MountConfig
mountConfigResult = lmap error <$> Mount.fromJSON <=< jsonResult

fileMetaResult ∷ String → Either Error Unit
fileMetaResult = map (\(_ ∷ JObject) → unit) <<< jsonResult

metastoreResult ∷ String → Either Error (Metastore.Metastore ())
metastoreResult = lmap error <$> Metastore.fromJSON <=< jsonResult

querySingleton ∷ String → String → URI.Query
querySingleton k v = URI.Query $ singleton $ Tuple k (Just v)

headerParams ∷ ∀ f. Foldable f ⇒ f (Tuple String String) → URI.Query
headerParams ps = querySingleton "request-headers" (show (foldl go jsonEmptyObject ps))
  where
  go ∷ Json → Tuple String String → Json
  go j (Tuple k v) = k := v ~> j

maxAgeHeader ∷ Seconds → Req.RequestHeader
maxAgeHeader (Seconds s) = Req.RequestHeader "Cache-Control" ("max-age=" <> show (Int.floor s))
