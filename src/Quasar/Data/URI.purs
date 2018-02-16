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
  ( QHierarchicalPart
  , QURIHost
  , QQuery
  , AbsPath
  , RelPath'
  , AnyPath
  , QAuthority
  , QAbsoluteURI
  , qAbsoluteURI
  , QRelativeRef
  , qRelativeRef
  , QURIRef
  , qURIRef
  , QURI
  , opts
  , regNameFromString
  , portFromInt
  , printScheme
  , unsafeSchemaFromString
  , unsafePortFromInt
  , unsafeRegNameFromString
  , module URI
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy (class SplitDirOrFile)
import Data.Path.Pathy (Abs, AbsPath, Name, viewDir, viewDirUnsandboxed, viewFile, viewFileUnsandboxed, Path, RelPath, Sandboxed, Unsandboxed, appendPath, parsePath, rootDir, runName, sandbox) as Py
import Data.Record.Builder as Builder
import Data.Tuple (Tuple(..))
import Data.URI (PathAbsolute, PathRootless, RegName)
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
import Data.URI.Host.RegName (fromString, unsafeFromString) as RegName
import Data.URI.Path (Path)
import Data.URI.Path (print) as Path
import Data.URI.Path.Absolute (print, PathAbsolute(..)) as PathAbsolute
import Data.URI.Path.NoScheme (print, PathNoScheme(..)) as PathNoScheme
import Data.URI.Path.Rootless (print) as PathRootless
import Data.URI.Path.Segment (PathSegment, PathSegmentNZ, segmentFromString, unsafeSegmentNZFromString, unsafeSegmentNZNCFromString)
import Data.URI.Port (fromInt, unsafeFromInt) as Port
import Data.URI.RelativeRef (RelativeRefOptions) as URI
import Data.URI.RelativeRef (print, parser, RelPath) as RelativeRef
import Data.URI.Scheme (Scheme) as URI
import Data.URI.Scheme (unsafeFromString, print) as Scheme
import Data.URI.URI (URIOptions) as URI
import Data.URI.URIRef (URIRefOptions) as URI
import Data.URI.URIRef (print, parser) as URIRef
import Text.Parsing.Parser (Parser)
import Type.Row (class RowListNub, class RowToList)

type AbsPath = Py.AbsPath Py.Sandboxed
type RelPath' = Py.RelPath Py.Unsandboxed
type AnyPath = Either AbsPath RelPath'
type QURIHost = URI.MultiHostPortPair URI.Host URI.Port
type QAuthority = URI.Authority URI.UserPassInfo QURIHost
type QQuery = URI.QueryPairs String String
type QHierarchicalPart = URI.HierarchicalPart   URI.UserPassInfo QURIHost AbsPath AbsPath

type QAbsoluteURI =        URI.AbsoluteURI        URI.UserPassInfo QURIHost AbsPath AbsPath QQuery
type QAbsoluteURIOptions = URI.AbsoluteURIOptions URI.UserPassInfo QURIHost AbsPath AbsPath QQuery

type QRelativeRef =        URI.RelativeRef        URI.UserPassInfo QURIHost AbsPath AnyPath QQuery URI.Fragment
type QRelativeRefOptions = URI.RelativeRefOptions URI.UserPassInfo QURIHost AbsPath AnyPath QQuery URI.Fragment

type QURIRef =             URI.URIRef        URI.UserPassInfo QURIHost AbsPath AbsPath AnyPath QQuery URI.Fragment
type QURIRefOptions =      URI.URIRefOptions URI.UserPassInfo QURIHost AbsPath AbsPath AnyPath QQuery URI.Fragment

type QURI =                URI.URI           URI.UserPassInfo QURIHost AbsPath AbsPath         QQuery URI.Fragment
type QURIOptions =         URI.URIOptions    URI.UserPassInfo QURIHost AbsPath AbsPath         QQuery URI.Fragment

type PrintParse from = { print :: from → String, parser :: Parser String from }
qAbsoluteURI ∷ PrintParse QAbsoluteURI
qAbsoluteURI = { print: AbsoluteURI.print opts.absoluteURI, parser: AbsoluteURI.parser opts.absoluteURI }
qRelativeRef ∷ PrintParse QRelativeRef
qRelativeRef = { print: RelativeRef.print opts.relativeRef, parser: RelativeRef.parser opts.relativeRef }
qURIRef ∷ PrintParse QURIRef
qURIRef = { print: URIRef.print opts.uriRef, parser: URIRef.parser opts.uriRef }

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
  parsePath = parseAbsSandboxedPath <<< Path.print
  printPath ∷ AbsPath → Path
  printPath = bimap Py.viewDir Py.viewFile >>>case _ of
    Left d ->
      URI.Path
        $ fromFoldable d 
        <#> Py.runName >>> segmentFromString
    Right (Tuple d n) -> 
      URI.Path
      $ (fromFoldable d <#> asSegment) <> [asSegment n]
      

  parseHierPath :: Either PathAbsolute PathRootless -> Either URIPartParseError AbsPath
  parseHierPath = parseAbsSandboxedPath <<< either PathAbsolute.print PathRootless.print
  printHierPath ∷ AbsPath → Either PathAbsolute PathRootless
  printHierPath = _printAbsPath >>> Left

  parseFragment :: URI.Fragment -> Either URIPartParseError URI.Fragment
  parseFragment = Right
  printFragment :: URI.Fragment -> URI.Fragment
  printFragment = id

  parseRelPath :: RelativeRef.RelPath -> Either URIPartParseError AnyPath
  parseRelPath = 
    bitraverse
      (PathAbsolute.print >>> parseAbsSandboxedPath)
      (PathNoScheme.print >>> parseRelUnsandboxedPath)
  
  
  printRelPath :: AnyPath -> RelativeRef.RelPath
  printRelPath = 
    bimap
      _printAbsPath
      _printRelPath

  _printAbsPath :: AbsPath → PathAbsolute
  _printAbsPath = bimap Py.viewDir Py.viewFile >>> case _ of
    Left Nil -> PathAbsolute.PathAbsolute Nothing
    Left (Cons head tail) -> PathAbsolute.PathAbsolute $ Just
      $ Tuple (asSegmentNZ head)
      $ (asSegment <$> fromFoldable tail)
    Right (Tuple d n) -> case d of
      Nil -> PathAbsolute.PathAbsolute $ Just $ Tuple (asSegmentNZ n) []
      Cons head tail -> PathAbsolute.PathAbsolute
        $ Just
        $ Tuple (asSegmentNZ head) 
        $ (asSegment <$> fromFoldable tail) <> [ asSegment n ]
  
  _printRelPath :: RelPath' → PathNoScheme.PathNoScheme
  _printRelPath = bimap Py.viewDirUnsandboxed Py.viewFileUnsandboxed >>> case _ of
    Left Nil -> PathNoScheme.PathNoScheme $ Tuple (unsafeSegmentNZNCFromString "./") []
    Left (Cons head tail) ->
    PathNoScheme.PathNoScheme
      $ Tuple (unsafeSegmentNZNCFromString $ maybe "../" Py.runName head)
      $ (segmentFromString <<< maybe "../" Py.runName <$> fromFoldable tail)
    Right (Tuple d n) -> case d of
      Nil -> PathNoScheme.PathNoScheme $ Tuple (unsafeSegmentNZNCFromString $ Py.runName n) []
      Cons head tail -> PathNoScheme.PathNoScheme
        $ Tuple (unsafeSegmentNZNCFromString $ maybe "../" Py.runName head)
        $ (segmentFromString <<< maybe "../" Py.runName <$> fromFoldable tail) <> [ asSegment n ]


  parseAbsSandboxedPath :: String -> Either URIPartParseError AbsPath
  parseAbsSandboxedPath = 
    Py.parsePath
      (const Nothing)
      (map Left <<< sandbox)
      (const Nothing)
      (map Right <<< sandbox)
      (const Nothing)
    >>> maybe (Left $ URIPartParseError "got invalid path") Right

  parseRelUnsandboxedPath :: String -> Either URIPartParseError RelPath'
  parseRelUnsandboxedPath = 
    Py.parsePath
      (Just <<< Left)
      (const Nothing)
      (Just <<< Right)
      (const Nothing)
      (const Nothing)
    >>> maybe (Left $ URIPartParseError "got invalid path") Right

  sandbox
    :: forall b s
    . SplitDirOrFile b
    => Py.Path Py.Abs b s
    -> Maybe (Py.Path Py.Abs b Py.Sandboxed)
  sandbox p = Py.appendPath Py.rootDir <$> Py.sandbox Py.rootDir p


printScheme :: URI.Scheme -> String
printScheme = Scheme.print

unsafeSchemaFromString :: String -> URI.Scheme
unsafeSchemaFromString = Scheme.unsafeFromString

regNameFromString :: String -> Maybe RegName
regNameFromString = RegName.fromString

unsafeRegNameFromString :: String -> RegName
unsafeRegNameFromString = RegName.unsafeFromString

unsafePortFromInt :: Int -> URI.Port
unsafePortFromInt = Port.unsafeFromInt

portFromInt :: Int -> Maybe URI.Port
portFromInt = Port.fromInt

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
asSegmentNZ = Py.runName >>> unsafeSegmentNZFromString

asSegment :: forall a. Py.Name a -> PathSegment
asSegment = Py.runName >>> segmentFromString 
