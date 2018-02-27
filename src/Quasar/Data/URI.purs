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

module Quasar.Data.URI 
  ( QAbsoluteURI
  , qAbsoluteURI
  , QRelativeRef
  , qRelativeRef
  , QURIRef
  , qURIRef
  , QHierarchicalPart
  , QURIHost
  , QQuery
  , AbsPath
  , AnyPath
  , QAuthority
  , opts
  , module URI
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.Codec (BasicCodec, basicCodec)
import Data.Either (Either(..), either, note)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Record.Builder as Builder
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Data.URI (PathAbsolute, PathRootless)
import Data.URI (URI(..), RelativePart(..), Authority(..), AbsoluteURI(..), HierarchicalPart(..), HierPath, Host(..), Path(..), Port, RelativeRef(..), URIRef, Fragment, Query, UserInfo) as URI
import Data.URI.AbsoluteURI (AbsoluteURIOptions) as URI
import Data.URI.AbsoluteURI (print, parser) as AbsoluteURI
import Data.URI.Common (URIPartParseError(..))
import Data.URI.Extra.MultiHostPortPair (MultiHostPortPair) as URI
import Data.URI.Extra.MultiHostPortPair (print, parser) as MultiHostPortPair
import Data.URI.Extra.QueryPairs (QueryPairs(..), Key, Value) as URI
import Data.URI.Extra.QueryPairs (print, parse, keyToString, valueToString, keyFromString, valueFromString) as QueryPairs
import Data.URI.Extra.UserPassInfo (UserPassInfo(..)) as URI
import Data.URI.Extra.UserPassInfo (print, parse) as UserPassInfo
import Data.URI.Path (Path)
import Data.URI.Path (print) as Path
import Data.URI.Path.Absolute (print, PathAbsolute(..)) as PathAbsolute
import Data.URI.Path.NoScheme (print, PathNoScheme(..)) as PathNoScheme
import Data.URI.Path.Rootless (print) as PathRootless
import Data.URI.Path.Segment (PathSegment, PathSegmentNZ, segmentFromString, unsafeSegmentNZFromString, unsafeSegmentNZNCFromString)
import Data.URI.RelativeRef (RelativeRefOptions) as URI
import Data.URI.RelativeRef (print, parser, RelPath) as RelativeRef
import Data.URI.Scheme (Scheme) as URI
import Data.URI.URI (URIOptions) as URI
import Data.URI.URIRef (URIRefOptions) as URI
import Data.URI.URIRef (print, parser) as URIRef
import Partial.Unsafe (unsafeCrashWith)
import Pathy (foldPath, posixParser)
import Pathy as Py
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Type.Row (class RowListNub, class RowToList)

type AbsPath = Py.AbsPath
type AnyPath = Either Py.AbsPath Py.RelPath

type QURIHost = URI.MultiHostPortPair URI.Host URI.Port
type QAuthority = URI.Authority URI.UserPassInfo QURIHost
type QQuery = URI.QueryPairs String String
type QHierarchicalPart = URI.HierarchicalPart URI.UserPassInfo QURIHost AbsPath AbsPath

type QAbsoluteURI = URI.AbsoluteURI URI.UserPassInfo QURIHost AbsPath AbsPath QQuery
type QAbsoluteURIOptions = URI.AbsoluteURIOptions URI.UserPassInfo QURIHost AbsPath AbsPath QQuery

type QRelativeRef = URI.RelativeRef URI.UserPassInfo QURIHost AbsPath AnyPath QQuery URI.Fragment
type QRelativeRefOptions = URI.RelativeRefOptions URI.UserPassInfo QURIHost AbsPath AnyPath QQuery URI.Fragment

type QURIRef = URI.URIRef URI.UserPassInfo QURIHost AbsPath AbsPath AnyPath QQuery URI.Fragment
type QURIRefOptions = URI.URIRefOptions URI.UserPassInfo QURIHost AbsPath AbsPath AnyPath QQuery URI.Fragment

-- type QURI = URI.URI URI.UserPassInfo QURIHost AbsPath AbsPath QQuery URI.Fragment
-- type QURIOptions = URI.URIOptions URI.UserPassInfo QURIHost AbsPath AbsPath QQuery URI.Fragment

qAbsoluteURI ∷ BasicCodec (Either ParseError) String QAbsoluteURI
qAbsoluteURI = basicCodec
  (flip runParser $ AbsoluteURI.parser opts.absoluteURI)
  (AbsoluteURI.print opts.absoluteURI)

qRelativeRef ∷ BasicCodec (Either ParseError) String QRelativeRef
qRelativeRef = basicCodec
  (flip runParser $ RelativeRef.parser opts.relativeRef)
  (RelativeRef.print opts.relativeRef)

qURIRef ∷ BasicCodec (Either ParseError) String QURIRef
qURIRef = basicCodec
  (flip runParser $ URIRef.parser opts.uriRef)
  (URIRef.print opts.uriRef)

opts :: 
  { absoluteURI ∷ Record QAbsoluteURIOptions
  , relativeRef ∷ Record QRelativeRefOptions
  , uriRef ∷ Record QURIRefOptions
  }
opts =
  { absoluteURI: _common `union` _Path `union` _HierPath
  , relativeRef: _common `union` _Path`union` _Fragment `union` _RelPath
  , uriRef: _common `union` _HierPath `union` _Path `union` _Fragment `union` _RelPath
  }
  where
  _common = _UserInfo `union` _Hosts `union` _Query

  _UserInfo = { parseUserInfo, printUserInfo }
  _Hosts = { parseHosts, printHosts }
  _Query = { parseQuery, printQuery }
  _Path = { parsePath, printPath }
  _HierPath = { parseHierPath, printHierPath }
  _Fragment = { parseFragment, printFragment }
  _RelPath = { parseRelPath, printRelPath }

  parseQuery :: URI.Query -> Either URIPartParseError QQuery
  parseQuery = QueryPairs.parse (QueryPairs.keyToString >>> pure) (QueryPairs.valueToString >>> pure)
  printQuery :: QQuery -> URI.Query
  printQuery = QueryPairs.print QueryPairs.keyFromString QueryPairs.valueFromString

  parseUserInfo :: URI.UserInfo -> Either URIPartParseError URI.UserPassInfo
  parseUserInfo = UserPassInfo.parse
  printUserInfo :: URI.UserPassInfo -> URI.UserInfo
  printUserInfo = UserPassInfo.print

  parseHosts :: Parser String QURIHost
  parseHosts = MultiHostPortPair.parser pure pure
  printHosts :: QURIHost -> String
  printHosts = MultiHostPortPair.print id id

  parsePath :: Path -> Either URIPartParseError AbsPath
  parsePath = _parseAbsPath <<< Path.print
  printPath ∷ AbsPath → Path
  printPath = bimap viewAbsDir viewAbsFile >>>case _ of
    Left d ->
      URI.Path
        $ (fromFoldable d <#> runName >>> segmentFromString) <> [ segmentFromString "" ]
    Right (Tuple d n) -> 
      URI.Path
      $ (fromFoldable d <#> asSegment) <> [asSegment n]
      

  parseHierPath :: Either PathAbsolute PathRootless -> Either URIPartParseError AbsPath
  parseHierPath = _parseAbsPath <<< either PathAbsolute.print PathRootless.print
  printHierPath ∷ AbsPath → Either PathAbsolute PathRootless
  printHierPath = _printAbsPath >>> Left

  parseFragment :: URI.Fragment -> Either URIPartParseError URI.Fragment
  parseFragment = Right
  printFragment :: URI.Fragment -> URI.Fragment
  printFragment = id

  printRelPath :: AnyPath -> RelativeRef.RelPath
  printRelPath = bimap _printAbsPath _printRelPath
  parseRelPath :: RelativeRef.RelPath -> Either URIPartParseError AnyPath
  parseRelPath = bitraverse
    (PathAbsolute.print >>> _parseAbsPath)
    (PathNoScheme.print >>> _parseRelPath)

  _printAbsPath :: Py.AbsPath → PathAbsolute
  _printAbsPath = bimap viewAbsDir viewAbsFile >>> case _ of
    Left Nil -> PathAbsolute.PathAbsolute Nothing
    Left (Cons head tail) -> PathAbsolute.PathAbsolute $ Just
      $ Tuple (asSegmentNZ head)
      $ (asSegment <$> fromFoldable tail) <> [ segmentFromString "" ]
    Right (Tuple d n) -> case d of
      Nil -> PathAbsolute.PathAbsolute $ Just $ Tuple (asSegmentNZ n) []
      Cons head tail -> PathAbsolute.PathAbsolute
        $ Just
        $ Tuple (asSegmentNZ head) 
        $ (asSegment <$> fromFoldable tail) <> [ asSegment n ]
  
  _printRelPath :: Py.RelPath → PathNoScheme.PathNoScheme
  _printRelPath = bimap viewRelDir viewRelFile >>> case _ of
    Left Nil -> PathNoScheme.PathNoScheme $ Tuple (unsafeSegmentNZNCFromString "./") []
    Left (Cons head tail) ->
      PathNoScheme.PathNoScheme
        $ Tuple (unsafeSegmentNZNCFromString $ maybe "../" runName head)
        $ (segmentFromString <<< maybe "../" runName <$> fromFoldable tail) <> [ segmentFromString "" ]

    Right (Tuple d n) -> case d of
      Nil -> PathNoScheme.PathNoScheme $ Tuple (unsafeSegmentNZNCFromString $ runName n) []
      Cons head tail -> PathNoScheme.PathNoScheme
        $ Tuple (unsafeSegmentNZNCFromString $ maybe "../" runName head)
        $ (segmentFromString <<< maybe "../" runName <$> fromFoldable tail) <> [ asSegment n ]


  _parseAbsPath :: String -> Either URIPartParseError Py.AbsPath
  _parseAbsPath =
    Py.parsePath posixParser
      (const Nothing)
      (Just <<< Left)
      (const Nothing)
      (Just <<< Right)
      Nothing
    >>> note (URIPartParseError "Could not parse valid absolute path")

  _parseRelPath :: String -> Either URIPartParseError Py.RelPath
  _parseRelPath =
    Py.parsePath posixParser
      (Just <<< Left)
      (const Nothing)
      (Just <<< Right)
      (const Nothing)
      Nothing
    >>> note (URIPartParseError "Could not parse valid relative path")

-- Union which rejects duplicates
union 
  :: forall r1 r2 r3 r3l
   . Union r1 r2 r3
  => RowToList r3 r3l
  => RowListNub r3l r3l
  => { | r1 }
  -> { | r2 }
  -> { | r3 }
union r1 r2 = Builder.build (Builder.merge r2) r1

asSegmentNZ :: forall a. Py.Name a -> PathSegmentNZ
asSegmentNZ = runName >>> unsafeSegmentNZFromString

asSegment :: forall a. Py.Name a -> PathSegment
asSegment = runName >>> segmentFromString 

runName :: forall a. Py.Name a -> String
runName = un Py.Name >>> NES.toString

viewAbsDir :: Py.Path Py.Abs Py.Dir -> List (Py.Name Py.Dir)
viewAbsDir = reverse <<< go
  where
  go p = foldPath Nil
    (\_ -> unsafeCrashWith "ParentOf node in viewDir")
    (flip Cons <<< go) p

viewAbsFile :: Py.Path Py.Abs Py.File -> Tuple (List (Py.Name Py.Dir)) (Py.Name Py.File)
viewAbsFile = Py.peelFile >>> lmap viewAbsDir


viewRelDir :: Py.Path Py.Rel Py.Dir -> List (Maybe (Py.Name Py.Dir))
viewRelDir = reverse <<< go
  where
  go p' = foldPath Nil
    (\p -> Cons Nothing (go p))
    (\p n -> Cons (Just n) (go p)) p'

viewRelFile :: Py.Path Py.Rel Py.File -> Tuple (List (Maybe (Py.Name Py.Dir))) (Py.Name Py.File)
viewRelFile = Py.peelFile >>> lmap viewRelDir

