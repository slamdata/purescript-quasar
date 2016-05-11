module Quasar.Advanced.Types where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>=>))

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pt
import Data.String as Str
import Data.Traversable (for, traverse)

import OIDCCryptUtils.JSONWebKey (JSONWebKey)
import OIDCCryptUtils.Types (Issuer(..), ClientID(..))

data Operation
  = Add
  | Read
  | Modify
  | Delete


instance encodeJsonOperation ∷ EncodeJson Operation where
  encodeJson Add = encodeJson "Add"
  encodeJson Read = encodeJson "Read"
  encodeJson Modify = encodeJson "Modify"
  encodeJson Delete = encodeJson "Delete"

instance decodeJsonOperation ∷ DecodeJson Operation where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "Add" → pure Add
      "Read" → pure Read
      "Modify" → pure Modify
      "Delete" → pure Delete
      _ → Left "Incorrect permission"


data ResourceType
  = Structural
  | Content

instance encodeJsonResourceType ∷ EncodeJson ResourceType where
  encodeJson Structural = encodeJson "Structural"
  encodeJson Content = encodeJson "Content"

instance decodeJsonResourceType ∷ DecodeJson ResourceType where
  decodeJson = decodeJson >=> case _ of
    "Structural" → pure Structural
    "Content" → pure Content
    _ → Left "Incorrect resource type"

data Resource
  = File (Pt.AbsFile Pt.Sandboxed)
  | Dir (Pt.AbsDir Pt.Sandboxed)
  | Group (Pt.AbsFile Pt.Sandboxed)

instance encodeJsonResource ∷ EncodeJson Resource where
  encodeJson (File pt) = encodeJson $ "file://" <> Pt.printPath pt
  encodeJson (Dir pt) = encodeJson $ "file://" <> Pt.printPath pt
  encodeJson (Group pt) = encodeJson $ "group://" <> Pt.printPath pt


instance decodeJsonResource ∷ DecodeJson Resource where
  decodeJson js = do
    str ← decodeJson js
    let
      groupPath = Str.stripPrefix "group://" str
      filePath = Str.stripPrefix "file://" str
    case groupPath, filePath of
      Nothing, Nothing → Left "Incorrect resource"
      Just pt, _ →
        map Group $ lmap (const $ "Incorrect group resource") $ parseFile pt
      _, Just pt →
        (map File $ lmap (const $ "Incorrect file resource") $ parseFile pt)
        <|>
        (map Dir $ lmap (const $ "Incorrect directory resource") $ parseDir pt)
parseFile ∷ String → Either String (Pt.AbsFile Pt.Sandboxed)
parseFile pt =
  Pt.parseAbsFile pt
  >>= Pt.sandbox Pt.rootDir
  <#> (Pt.rootDir </> _)
  # maybe (Left "Incorrect resource") pure

parseDir ∷ String → Either String (Pt.AbsDir Pt.Sandboxed)
parseDir pt =
  Pt.parseAbsDir pt
  >>= Pt.sandbox Pt.rootDir
  <#> (Pt.rootDir </> _)
  # maybe (Left "Incorrect resource") pure

type ActionR =
  { operation ∷ Operation
  , resource ∷ Resource
  , resourceType ∷ ResourceType
  }

newtype Action = Action ActionR
runAction ∷ Action → ActionR
runAction (Action r) = r

instance encodeJsonAction ∷ EncodeJson Action where
  encodeJson (Action obj) =
    "operation" := obj.operation
    ~> "resource" := obj.resource
    ~> "resourceType" := obj.resourceType
    ~> jsonEmptyObject

instance decodeJsonAction ∷ DecodeJson Action where
  decodeJson = decodeJson >=> \obj →
    { operation: _
    , resource: _
    , resourceType: _
    }
    <$> (obj .? "operation")
    <*> (obj .? "resource")
    <*> (obj .? "resourceType")
    <#> Action


newtype UserId = UserId String
runUserId ∷ UserId → String
runUserId (UserId s) = s

instance encodeJsonUserId ∷ EncodeJson UserId where
  encodeJson = runUserId >>> encodeJson

instance decodeJsonUserId ∷ DecodeJson UserId where
  decodeJson = map UserId <<< decodeJson


newtype TokenId = TokenId String
runTokenId ∷ TokenId → String
runTokenId (TokenId s) = s

instance encodeJsonTokenId ∷ EncodeJson TokenId where
  encodeJson = runTokenId >>> encodeJson

instance decodeJsonTokenId ∷ DecodeJson TokenId where
  decodeJson = map TokenId <<< decodeJson

newtype PermissionId = PermissionId String
runPermissionId ∷ PermissionId → String
runPermissionId (PermissionId s) = s

instance encodeJsonPermissionId ∷ EncodeJson PermissionId where
  encodeJson = runPermissionId >>> encodeJson

instance decodeJsonPermissionId ∷ DecodeJson PermissionId where
  decodeJson = map PermissionId <<< decodeJson


data GrantedTo
  = UserGranted UserId
  | GroupGranted (Pt.AbsFile Pt.Sandboxed)
  | TokenGranted TokenId

instance encodeJsonGrantedTo ∷ EncodeJson GrantedTo where
  encodeJson (UserGranted uid) = encodeJson uid
  encodeJson (GroupGranted pt) = encodeJson $ Pt.printPath pt
  encodeJson (TokenGranted tk) = encodeJson tk

instance decodeJsonGrantedTo ∷ DecodeJson GrantedTo where
  decodeJson = decodeJson >=> \str →
    (map GroupGranted $ parseFile str)
    <|>
    (map (UserGranted <<< UserId) $ checkUserId str)
    <|>
    (pure $ TokenGranted $ TokenId str)
    where
    checkUserId ∷ String → Either String String
    checkUserId str =
      -- Right now we don't know what's exact format of TokenId
      -- I suggest not having `@` should be enough to say that
      -- string isn't email.
      if isJust (Str.indexOf "@" str)
        then pure str
        else Left "Incorrect email"

type GrantedByR =
  { tokens ∷ Array TokenId
  , users ∷ Array UserId
  , groups ∷ Array (Pt.AbsFile Pt.Sandboxed)
  }

newtype GrantedBy = GrantedBy GrantedByR
runGrantedBy ∷ GrantedBy → GrantedByR
runGrantedBy (GrantedBy r) = r

instance encodeJsonGrantedBy ∷ EncodeJson GrantedBy where
  encodeJson (GrantedBy obj) =
    "tokens" := obj.tokens
    ~> "users" := obj.users
    ~> "groups" := map (append "group://" <<< Pt.printPath) obj.groups
    ~> jsonEmptyObject

instance decodeJsonGrantedBy ∷ DecodeJson GrantedBy where
  decodeJson = decodeJson >=> \obj →
    { tokens: _
    , users: _
    , groups: _
    }
    <$> (obj .? "tokens")
    <*> (obj .? "users")
    <*> ((obj .? "groups") >>= extractGroups)
    <#> GrantedBy
    where
    extractGroups ∷ Array String → Either String (Array (Pt.AbsFile Pt.Sandboxed))
    extractGroups as = do
      woSchema ←
        for as $ (Str.stripPrefix "group://" >>> maybe (Left "Incorrect group") pure)
      for woSchema parseFile


type PermissionR =
  { id ∷ PermissionId
  , action ∷ Action
  , grantedTo ∷ GrantedTo
  , grantedBy ∷ GrantedByR
  }

newtype Permission = Permission PermissionR
runPermission ∷ Permission → PermissionR
runPermission (Permission r) = r

instance decodeJsonPermission ∷ DecodeJson Permission where
  decodeJson = decodeJson >=> \obj →
    { id: _
    , action: _
    , grantedTo: _
    , grantedBy: _
    }
    <$> (obj .? "id")
    <*> (obj .? "action")
    <*> (obj .? "grantedTo")
    <*> ((obj .? "grantedBy") <#> runGrantedBy)
    <#> Permission


type GroupInfoR =
  { members ∷ Array UserId
  , allMembers ∷ Array UserId
  , subGroups ∷ Array (Pt.RelFile Pt.Sandboxed)
  }

newtype GroupInfo = GroupInfo GroupInfoR
runGroupInfo ∷ GroupInfo → GroupInfoR
runGroupInfo (GroupInfo r) = r

instance decodeJsonGroupInfo ∷ DecodeJson GroupInfo where
  decodeJson = decodeJson >=> \obj →
    { members: _
    , allMembers: _
    , subGroups: _
    }
    <$> (obj .? "members")
    <*> (obj .? "allMembers")
    <*> ((obj .? "subGroups") >>= extractGroups)
    <#> GroupInfo
    where
    extractGroups ∷ Array String → Either String (Array (Pt.RelFile Pt.Sandboxed))
    extractGroups =
      traverse (\x → maybe (Left "Incorrect subgroup") pure
                     $ Pt.parseRelFile x
                     >>= Pt.sandbox Pt.currentDir)


type GroupPatchR =
  { addUsers ∷ Array UserId
  , removeUsers ∷ Array UserId
  }

newtype GroupPatch = GroupPatch GroupPatchR
runGroupPatch ∷ GroupPatch → GroupPatchR
runGroupPatch (GroupPatch r) = r

instance encodeJsonGroupPatch ∷ EncodeJson GroupPatch where
  encodeJson (GroupPatch obj) =
    "addUsers" := obj.addUsers
    ~> "removeUsers" := obj.removeUsers
    ~> jsonEmptyObject


type ShareRequestR =
  { users ∷ Array UserId
  , groups ∷ Array (Pt.AbsFile Pt.Sandboxed)
  , actions ∷ Array ActionR
  }

newtype ShareRequest = ShareRequest ShareRequestR
runShareRequest ∷ ShareRequest → ShareRequestR
runShareRequest (ShareRequest r) = r

instance encodeJsonShareRequest ∷ EncodeJson ShareRequest where
  encodeJson (ShareRequest obj) =
    "users" := obj.users
    ~> "groups" := map (append "group://" <<< Pt.printPath) obj.groups
    ~> "actions" := (map Action $ obj.actions)
    ~> jsonEmptyObject


newtype TokenHash = TokenHash String
runTokenHash ∷ TokenHash → String
runTokenHash (TokenHash s) = s

instance encodeJsonTokenHash ∷ EncodeJson TokenHash where
  encodeJson = runTokenHash >>> encodeJson

instance decodeJsonTokenHash ∷ DecodeJson TokenHash where
  decodeJson = map TokenHash <<< decodeJson


newtype TokenName = TokenName String
runTokenName ∷ TokenName → String
runTokenName (TokenName s) = s

instance encodeJsonTokenName ∷ EncodeJson TokenName where
  encodeJson = runTokenName >>> encodeJson

instance decodeJsonTokenName ∷ DecodeJson TokenName where
  decodeJson = map TokenName <<< decodeJson


type TokenR =
  { id ∷ TokenId
  , secret ∷ Maybe TokenHash
  , name ∷ Maybe TokenName
  , grantedBy ∷ GrantedByR
  , actions ∷ Array ActionR
  }

newtype Token = Token TokenR
runToken ∷ Token → TokenR
runToken (Token r) = r

instance decodeJsonToken ∷ DecodeJson Token where
  decodeJson = decodeJson >=> \obj →
    { id: _
    , secret: _
    , name: _
    , grantedBy: _
    , actions: _
    }
    <$> (obj .? "id")
    <*> ((obj .? "secret") <|> pure Nothing)
    <*> ((obj .? "name") <|> pure Nothing)
    <*> (obj .? "grantedBy" <#> runGrantedBy)
    <*> (obj .? "actions" <#> (map runAction))
    <#> Token


type OpenIDConfigurationR =
  { issuer ∷ Issuer
  , authorizationEndpoint ∷ String
  , tokenEndpoint ∷ String
  , userinfoEndpoint ∷ String
  , jwks ∷ Array JSONWebKey
  }

newtype OpenIDConfiguration = OpenIDConfiguration OpenIDConfigurationR
runOpenIDConfiguration ∷ OpenIDConfiguration → OpenIDConfigurationR
runOpenIDConfiguration (OpenIDConfiguration r) = r

instance decodeJSONOIDC ∷ DecodeJson OpenIDConfiguration where
  decodeJson = decodeJson >=> \obj → do
    issuer ← Issuer <$> obj .? "issuer"
    authorizationEndpoint ← obj .? "authorization_endpoint"
    tokenEndpoint ← obj .? "token_endpoint"
    userinfoEndpoint ← obj .? "userinfo_endpoint"
    jwks ← obj .? "jwks"
    pure
      $ OpenIDConfiguration
          { issuer, authorizationEndpoint, tokenEndpoint, userinfoEndpoint, jwks }

type ProviderR =
  { displayName ∷ String
  , clientID ∷ ClientID
  , openIDConfiguration ∷ OpenIDConfigurationR
  }

newtype Provider = Provider ProviderR
runProvider ∷ Provider → ProviderR
runProvider (Provider r) = r

instance decodeJsonProvider ∷ DecodeJson Provider where
  decodeJson = decodeJson >=> \obj → do
    displayName ← obj .? "display_name"
    clientID ← ClientID <$> obj .? "client_id"
    openIDConfiguration ← obj .? "openid_configuration" <#> runOpenIDConfiguration
    pure $ Provider { displayName, clientID, openIDConfiguration }
