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
import Data.Traversable (traverse)

import OIDC.Crypt.JSONWebKey (JSONWebKey)
import OIDC.Crypt.Types (Issuer(..), ClientID(..), runClientID, runIssuer)

data Operation
  = Add
  | Read
  | Modify
  | Delete

derive instance eqOperation ∷ Eq Operation
derive instance ordOperation ∷ Ord Operation

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


data AccessType
  = Structural
  | Content
  | Mount

derive instance eqAccessType ∷ Eq AccessType
derive instance ordAccessType ∷ Ord AccessType

instance encodeJsonAccessType ∷ EncodeJson AccessType where
  encodeJson Structural = encodeJson "Structural"
  encodeJson Content = encodeJson "Content"
  encodeJson Mount = encodeJson "Mount"

instance decodeJsonAccessType ∷ DecodeJson AccessType where
  decodeJson = decodeJson >=> case _ of
    "Structural" → pure Structural
    "Content" → pure Content
    "Mount" → pure Mount
    _ → Left "Incorrect resource type"


data Resource
  = File (Pt.AbsFile Pt.Sandboxed)
  | Dir (Pt.AbsDir Pt.Sandboxed)
  | Group (Pt.AbsFile Pt.Sandboxed)

derive instance eqResource ∷ Eq Resource
derive instance ordResource ∷ Ord Resource

instance encodeJsonResource ∷ EncodeJson Resource where
  encodeJson (File pt) = encodeJson $ "data:" <> Pt.printPath pt
  encodeJson (Dir pt) = encodeJson $ "data:" <> Pt.printPath pt
  encodeJson (Group pt) = encodeJson $ "group:" <> Pt.printPath pt

instance decodeJsonResource ∷ DecodeJson Resource where
  decodeJson js = do
    str ← decodeJson js
    let
      groupPath = Str.stripPrefix "group:" str
      filePath = Str.stripPrefix "data:" str
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
  , accessType ∷ AccessType
  }

newtype Action = Action
  { operation ∷ Operation
  , resource ∷ Resource
  , accessType ∷ AccessType
  }

runAction ∷ Action → ActionR
runAction (Action r) = r

derive instance eqAction ∷ Eq Action
derive instance ordAction ∷ Ord Action

instance encodeJsonAction ∷ EncodeJson Action where
  encodeJson (Action obj) =
    "operation" := obj.operation
    ~> "resource" := obj.resource
    ~> "accessType" := obj.accessType
    ~> jsonEmptyObject

instance decodeJsonAction ∷ DecodeJson Action where
  decodeJson = decodeJson >=> \obj →
    { operation: _
    , resource: _
    , accessType: _
    }
    <$> (obj .? "operation")
    <*> (obj .? "resource")
    <*> (obj .? "accessType")
    <#> Action

newtype UserId = UserId String

runUserId ∷ UserId → String
runUserId (UserId s) = s

derive instance eqUserId ∷ Eq UserId
derive instance ordUserId ∷ Ord UserId

instance encodeJsonUserId ∷ EncodeJson UserId where
  encodeJson = encodeJson <<< runUserId

instance decodeJsonUserId ∷ DecodeJson UserId where
  decodeJson = map UserId <<< decodeJson


newtype TokenId = TokenId String

runTokenId ∷ TokenId → String
runTokenId (TokenId s) = s

derive instance eqTokenId ∷ Eq TokenId
derive instance ordTokenId ∷ Ord TokenId

instance encodeJsonTokenId ∷ EncodeJson TokenId where
  encodeJson = runTokenId >>> encodeJson

instance decodeJsonTokenId ∷ DecodeJson TokenId where
  decodeJson = map TokenId <<< decodeJson

newtype PermissionId = PermissionId String
runPermissionId ∷ PermissionId → String
runPermissionId (PermissionId s) = s

derive instance eqPermissionId ∷ Eq PermissionId
derive instance ordPermissionId ∷ Ord PermissionId

instance encodeJsonPermissionId ∷ EncodeJson PermissionId where
  encodeJson = encodeJson <<< runPermissionId

instance decodeJsonPermissionId ∷ DecodeJson PermissionId where
  decodeJson = map PermissionId <<< decodeJson


data GrantedTo
  = UserGranted UserId
  | GroupGranted (Pt.AbsFile Pt.Sandboxed)
  | TokenGranted TokenId

derive instance eqGrantedTo ∷ Eq GrantedTo
derive instance ordGrantedTo ∷ Ord GrantedTo

instance encodeJsonGrantedTo ∷ EncodeJson GrantedTo where
  encodeJson (UserGranted uid) = encodeJson uid
  encodeJson (GroupGranted pt) = encodeJson $ Pt.printPath pt
  encodeJson (TokenGranted tk) = encodeJson tk

instance decodeJsonGrantedTo ∷ DecodeJson GrantedTo where
  decodeJson j =
    (decodeJson j >>= parseGroup <#> GroupGranted)
    <|>
    (decodeJson j >>= checkUserId >>= parseUserId  <#> UserGranted)
    <|>
    (decodeJson j >>= parseTokenId <#> TokenGranted)
    where
    checkUserId ∷ String → Either String String
    checkUserId str =
      -- Right now we don't know what's exact format of TokenId
      -- I suggest not having `@` should be enough to say that
      -- string isn't email.
      if isJust (Str.indexOf "@" str)
        then pure str
        else Left "Incorrect email"

    parseUserId ∷ String → Either String UserId
    parseUserId str =
      Str.stripPrefix "user:" str # maybe (Left "Incorrect user") (pure <<< UserId)

    parseTokenId ∷ String → Either String TokenId
    parseTokenId str =
      Str.stripPrefix "token:" str # maybe (Left "Incorrect token") (pure <<< TokenId)

parseGroup ∷ String → Either String (Pt.AbsFile Pt.Sandboxed)
parseGroup string =
  Str.stripPrefix "group:" string
  # maybe (Left "Incorrect group") pure
  >>= parseFile


type PermissionR =
  { id ∷ PermissionId
  , action ∷ ActionR
  , grantedTo ∷ GrantedTo
  }

newtype Permission = Permission PermissionR
runPermission ∷ Permission → PermissionR
runPermission (Permission r) = r

instance decodeJsonPermission ∷ DecodeJson Permission where
  decodeJson =  decodeJson >=> \obj →
    { id: _
    , action: _
    , grantedTo: _
    }
    <$> (obj .? "id")
    <*> ((obj .? "action") <#> runAction)
    <*> (obj .? "grantedTo")
    <#> Permission


type GroupInfoR =
  { members ∷ Array UserId
  , allMembers ∷ Array UserId
  , subGroups ∷ Array (Pt.AbsFile Pt.Sandboxed)
  }

newtype GroupInfo = GroupInfoN GroupInfoR

runGroupInfo ∷ GroupInfo → GroupInfoR
runGroupInfo (GroupInfoN r) = r

instance decodeJsonGroupInfo ∷ DecodeJson GroupInfo where
  decodeJson = decodeJson >=> \obj →
    { members: _
    , allMembers: _
    , subGroups: _
    }
    <$> (obj .? "members")
    <*> (obj .? "allMembers")
    <*> ((obj .? "subGroups") >>= extractGroups)
    <#> GroupInfoN
    where
    extractGroups ∷ Array String → Either String (Array (Pt.AbsFile Pt.Sandboxed))
    extractGroups =
      traverse (\x → maybe (Left "Incorrect subgroup") pure
                     $ Pt.parseAbsFile x
                     >>= Pt.sandbox Pt.rootDir
                     <#> (\y → Pt.rootDir </> y)
               )


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

data ShareableSubject
  = UserSubject UserId
  | GroupSubject (Pt.AbsFile Pt.Sandboxed)

instance encodJsonShareableSubject ∷ EncodeJson ShareableSubject where
  encodeJson (UserSubject (UserId uid)) =
    encodeJson $ "user:" <> uid
  encodeJson (GroupSubject fp) =
    encodeJson $ "group:" <> Pt.printPath fp


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
    "subjects" := ((map (append "user:" <<< runUserId) obj.users)
                   <> map (append "group:" <<< Pt.printPath) obj.groups)
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

derive instance eqTokenName ∷ Eq TokenName
derive instance ordTokenName ∷ Ord TokenName

type TokenR =
  { id ∷ TokenId
  , secret ∷ TokenHash
  , name ∷ Maybe TokenName
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
    , actions: _
    }
    <$> (obj .? "id")
    <*> (obj .? "secret")
    <*> (((obj .? "name")
            >>= \x → if runTokenName x == ""
                     then pure Nothing
                     else pure $ Just x)
         <|> pure Nothing)
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

instance encodeJsonOIDC ∷ EncodeJson OpenIDConfiguration where
  encodeJson (OpenIDConfiguration obj) =
    "issuer" := runIssuer obj.issuer
    ~> "authorization_endpoint" := obj.authorizationEndpoint
    ~> "token_endpoint" := obj.tokenEndpoint
    ~> "userinfo_endpoint" := obj.userinfoEndpoint
    ~> "jwks" := obj.jwks
    ~> jsonEmptyObject

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

instance encodeJsonProvider ∷ EncodeJson Provider where
  encodeJson (Provider obj) =
    "display_name" := obj.displayName
    ~> "client_id" := runClientID obj.clientID
    ~> "openid_configuration" := OpenIDConfiguration obj.openIDConfiguration
    ~> jsonEmptyObject
