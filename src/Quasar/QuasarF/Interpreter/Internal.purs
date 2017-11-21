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
  , mkURL
  , mkRequest
  ) where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Free (Free, liftF)

import Data.Argonaut as Json
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Foldable (oneOf)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (Sandboxed, Rel, Path, Abs, RelDir, file, dir, printPath, rootDir, relativeTo, (</>))
import Data.String as Str
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Global (encodeURIComponent)

import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.AffjaxF as AXF
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.ResponseHeader as RH

import Quasar.ConfigF as CF
import Quasar.QuasarF (QError(..), PDFError(..), UnauthorizedDetails(..), AnyPath, Pagination)

type AXFP = AXF.AffjaxFP RequestContent String

ask ∷ ∀ c r. Free (Coproduct (CF.ConfigF c) r) c
ask = liftF $ left $ CF.GetConfig id

jsonResult ∷ ∀ j. Json.DecodeJson j ⇒ String → Either Error j
jsonResult = lmap error <$> (Json.decodeJson <=< Json.jsonParser)

strResult ∷ String → Either Error String
strResult = Right

unitResult ∷ String → Either Error Unit
unitResult = const (Right unit)

toVarParams ∷ SM.StrMap String → List (Tuple String String)
toVarParams = map (lmap ("var." <> _)) <<< SM.toUnfoldable

toPageParams ∷ Maybe Pagination → List (Tuple String String)
toPageParams Nothing = Nil
toPageParams (Just { offset, limit })
  = Tuple "offset" (show offset)
  : Tuple "limit" (show limit)
  : Nil

defaultRequest ∷ AX.AffjaxRequest RequestContent
defaultRequest = AX.defaultRequest { content = Nothing }

get ∷ AX.URL → AXF.AffjaxF RequestContent String
get u = AXF.affjax (defaultRequest { url = u })

put ∷ AX.URL → RequestContent → AXF.AffjaxF RequestContent String
put u c = AXF.affjax (defaultRequest { method = Left PUT, url = u, content = Just c })

delete ∷ AX.URL → AXF.AffjaxF RequestContent String
delete u = AXF.affjax (defaultRequest { method = Left DELETE, url = u })

mkURL
  ∷ ∀ r
  . RelDir Sandboxed
  → AnyPath
  → List (Tuple String String)
  → Free (Coproduct (CF.ConfigF { basePath ∷ String | r }) AXFP) String
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
  $ Str.joinWith "/"
  $ map encodeURIComponent
  $ Str.split (Str.Pattern "/")
  $ either printPath printPath
  $ bimap (baseify (dir "/")) (baseify (file "")) fsPath
  where
  baseify
    ∷ ∀ b. Path Rel b Sandboxed → Path Abs b Sandboxed → Path Rel b Sandboxed
  baseify x p = base </> fromMaybe x (p `relativeTo` rootDir)

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
    ])
  where
    wrapError ∷ Json.Json → Json.JObject
    wrapError = SM.singleton "error"
