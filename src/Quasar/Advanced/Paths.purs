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

module Quasar.Advanced.Paths where

import Data.Path.Pathy (RelDir, RelFile, Sandboxed, file, dir, (</>))

oidcProviders ∷ RelFile Sandboxed
oidcProviders = dir "security" </> dir "oidc" </> file "providers"

token ∷ RelDir Sandboxed
token = dir "security" </> dir "token"

group ∷ RelDir Sandboxed
group = dir "security" </> dir "group"

permission ∷ RelDir Sandboxed
permission = dir "security" </> dir "permission"

authority ∷ RelDir Sandboxed
authority = dir "security" </> dir "authority"

licenseInfo ∷ RelFile Sandboxed
licenseInfo = dir "server" </> file "licenseInfo"

licensee ∷ RelFile Sandboxed
licensee = dir "server" </> file "licensee"
