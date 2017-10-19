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

module Quasar.Mount.View where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (~>), (:=))
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List ((:), List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.String as Str
import Data.Tuple (Tuple(..), lookup)
import Data.URI as URI
import Data.URI.AbsoluteURI as AbsoluteURI
import Quasar.Types (Vars)
import SqlSquared (SqlQuery)
import SqlSquared as Sql
import Text.Parsing.Parser (ParseError(..))
import Text.Parsing.Parser.Pos (Position(..))

type Config =
  { query ∷ SqlQuery
  , vars ∷ Vars
  }

toJSON ∷ Config → Json
toJSON config =
  let uri = AbsoluteURI.print (toURI config)
  in "view" := ("connectionUri" := uri ~> jsonEmptyObject) ~> jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< AbsoluteURI.parse
  <=< (_ .? "connectionUri")
  <=< (_ .? "view")
  <=< decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { query, vars } =
  URI.AbsoluteURI
    (Just uriScheme)
    (URI.HierarchicalPart Nothing Nothing)
    (Just (URI.Query props))
  where
  props ∷ List (Tuple String (Maybe String))
  props
    = Tuple "q" (Just $ Sql.printQuery query)
    : (bimap ("var." <> _) Just <$> SM.toUnfoldable vars)

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme _ query) = do
  unless (scheme == Just uriScheme) $ Left "Expected 'sql2' URL scheme"
  let queryMap = maybe List.Nil (\(URI.Query q) → q) query
  query' ← maybe (Left "Expected 'q' query variable") pure (extractQuery queryMap)
  q ← Sql.parseQuery query' # lmap \(ParseError err (Position { line , column })) →
    "Expected 'q' query variable to contain valid query, " <> "but at line "
    <> show line <> "and column " <> show column <> " got parse error: \n" <> err
  let vars = SM.fromFoldable $ foldMap extractVar queryMap
  pure { query: q, vars }

uriScheme ∷ URI.Scheme
uriScheme = URI.Scheme "sql2"

extractQuery ∷ List (Tuple String (Maybe String)) → Maybe String
extractQuery= join <<< lookup "q"

extractVar ∷ Tuple String (Maybe String) → List (Tuple String String)
extractVar (Tuple key val) = maybe Nil List.singleton $
  Tuple <$> Str.stripPrefix (Str.Pattern "var.") key <*> val
