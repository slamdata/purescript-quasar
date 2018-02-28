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

module Quasar.QuasarF.Interpreter.Internal
  ( ask
  , jsonResult
  , strResult
  , unitResult
  , toVarParams
  , toPageParams
  , defaultRequest
  , get
  , put
  , delete
  , mkUrl
  , mkFSUrl
  , mkRequest
  ) where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Free (Free, liftF)
import Data.Argonaut as Json
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Codec (encode)
import Data.Either (Either(..), either)
import Data.Foldable (oneOf)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.String as Str
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.ResponseHeader as RH
import Network.HTTP.StatusCode (StatusCode(..))
import Pathy (class IsDirOrFile, Abs, AbsPath, Path, Rel, RelDir, RelPath, relativeTo, rootDir, (</>))
import Quasar.ConfigF as CF
import Quasar.QuasarF (Pagination, QError(..), PDFError(..), UnauthorizedDetails(..))
import Quasar.QuasarF.Interpreter.Config (Config)
import Quasar.URI as URI

type AXFP = AXF.AffjaxFP RequestContent String
type AjaxM r a = Free (Coproduct (CF.ConfigF (Config r)) AXFP) a

ask ∷ ∀ c r. Free (Coproduct (CF.ConfigF c) r) c
ask = liftF $ left $ CF.GetConfig id

jsonResult ∷ ∀ j. Json.DecodeJson j ⇒ String → Either Error j
jsonResult = lmap error <$> (Json.decodeJson <=< Json.jsonParser)

strResult ∷ String → Either Error String
strResult = Right

unitResult ∷ String → Either Error Unit
unitResult = const (Right unit)

toVarParams ∷ SM.StrMap String → URI.QQuery
toVarParams = URI.QueryPairs <<< map (bimap ("var." <> _) Just) <<< SM.toUnfoldable

toPageParams ∷ Maybe Pagination → URI.QQuery
toPageParams Nothing = mempty
toPageParams (Just { offset, limit }) = URI.QueryPairs
  [ Tuple "offset" (Just (show offset))
  , Tuple "limit" (Just (show limit))
  ]

defaultRequest ∷ AX.AffjaxRequest RequestContent
defaultRequest = AX.defaultRequest { content = Nothing }

get ∷ AX.URL → AXF.AffjaxF RequestContent String
get u = AXF.affjax (defaultRequest { url = u })

put ∷ AX.URL → RequestContent → AXF.AffjaxF RequestContent String
put u c = AXF.affjax (defaultRequest { method = Left PUT, url = u, content = Just c })

delete ∷ AX.URL → AXF.AffjaxF RequestContent String
delete u = AXF.affjax (defaultRequest { method = Left DELETE, url = u })

mkFSUrl
  ∷ ∀ r
  . RelDir
  → AbsPath
  → URI.QQuery
  → AjaxM r String
mkFSUrl relDir fsPath q = mkUrl (bimap baseify baseify fsPath) q
  where
    baseify ∷ ∀ b. IsDirOrFile b => Path Abs b → Path Rel b
    baseify p = relDir </> p `relativeTo` rootDir

mkUrl ∷ ∀ r. RelPath → URI.QQuery → AjaxM r String
mkUrl relPath q = encode URI.qURIRef <$> mkUrl' relPath q

mkUrl' ∷ ∀ r. RelPath → URI.QQuery → AjaxM r URI.QURIRef
mkUrl' relPath q = do
  { basePath } ← ask
  pure (bimap toURI toRelativeRef basePath)
  where
    toURI { scheme, authority, path } =
      let
        hierPath = (Just (bimap (path </> _) (path </> _) relPath))
        hierPart = case authority of
          Nothing -> URI.HierarchicalPartNoAuth hierPath
          Just authority' -> URI.HierarchicalPartAuth authority' hierPath
        query = if q == mempty then Nothing else Just q
      in URI.URI scheme hierPart query Nothing

    toRelativeRef :: RelDir -> URI.QRelativeRef
    toRelativeRef relDir =
      URI.RelativeRef
        (URI.RelativePartNoAuth
          (Just $ Right (bimap (relDir </> _) (relDir </> _) relPath)))
        (if q == mempty then Nothing else Just q)
        Nothing


mkRequest
  ∷ ∀ a l
  . (String → Either Error a)
  → AXF.AffjaxF RequestContent String
  → Free (Coproduct l AXFP) (Either QError a)
mkRequest f = map (handleResult f) <<< liftF <<< right

handleResult
  ∷ ∀ a
  . (String → Either Error a)
  → Either Error (AX.AffjaxResponse String)
  → Either QError a
handleResult f =
  case _ of
    Right { status: StatusCode code, response, headers }
      | code >= 200 && code < 300 → lmap Error (f response)
      | code == 404 → Left NotFound
      | code == 403 → Left Forbidden
      | code == 402 → Left PaymentRequired
      | code == 401 →
          Left
            $ Unauthorized
            $ (UnauthorizedDetails <<< show)
            <$> (Array.index headers =<< Array.findIndex isWWWAuthenticate headers)
      | otherwise →
          let
            parseResult = parseHumanReadableError =<< hush (Json.decodeJson =<< Json.jsonParser response)
            fallbackError = Error $ error $ "An unknown error ocurred: " <> show code <> " " <> show response
          in
            Left (fromMaybe fallbackError parseResult)
    Left err → Left (Error err)
  where
  isWWWAuthenticate ∷ RH.ResponseHeader → Boolean
  isWWWAuthenticate = eq "www-authenticate" <<< Str.toLower <<< RH.responseHeaderName

hush ∷ ∀ a b. Either a b → Maybe b
hush = either (const Nothing) Just

-- | Try to parse the known Quasar error formats, to get at a human readable error message
parseHumanReadableError ∷ Json.JObject → Maybe QError
parseHumanReadableError json =
  oneOf (map hush
    [ do message ← json .? "error"
         pure (ErrorMessage {title: Nothing, message, raw: json})
    , do e ← json .? "error"
         message ← e .? "message"
         pure (ErrorMessage {title: Nothing, message, raw: json})
    , do e ← json .? "error"
         detail ← e .? "detail"
         title ← e .?? "status"
         message ← detail .? "message"
         pure (ErrorMessage {title, message, raw: json})
    , do e ← json .? "error"
         mErr ← e .? "status"
         case mErr of
           "Multiple errors" → do
             detail ← e .? "detail"
             errors ← detail .? "errors"
             pure
               $ MultipleErrors
               $ Array.catMaybes
               $ map (parseHumanReadableError <<< wrapError) errors
           _ → do
             Left "Parse error in multiple errors"
    , do detail ← json .? "detail"
         pdfError ← detail .? "PDFError"
         code ← pdfError .? "code"
         case code of
           "CEF_PDF_ERROR" → do
             reason ← pdfError .? "reason"
             pure $ PDFError $ CEFPDFError reason
           "NO_CEF_PATH_IN_CONFIG" →
             pure $ PDFError NoCEFPathInConfig
           _ →
             Left "Unexpected PDF code"
    ])
  where
    wrapError ∷ Json.Json → Json.JObject
    wrapError = SM.singleton "error"
