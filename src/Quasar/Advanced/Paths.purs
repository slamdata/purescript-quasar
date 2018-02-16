{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file_ except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Quasar.Advanced.Paths where

import Data.Path.Pathy (RelDir, RelFile, Sandboxed, (</>))
import Data.Symbol (SProxy(..))
import Quasar.Internal (dir_, file_)

oidcProviders ∷ RelFile Sandboxed
oidcProviders = dir_ (SProxy :: SProxy "security") </> dir_ (SProxy :: SProxy "oidc") </> file_ (SProxy :: SProxy "providers")

token ∷ RelDir Sandboxed
token = dir_ (SProxy :: SProxy "security") </> dir_ (SProxy :: SProxy "token")

group ∷ RelDir Sandboxed
group = dir_ (SProxy :: SProxy "security") </> dir_ (SProxy :: SProxy "group")

permission ∷ RelDir Sandboxed
permission = dir_ (SProxy :: SProxy "security") </> dir_ (SProxy :: SProxy "permission")

children ∷ RelFile Sandboxed
children = file_ (SProxy :: SProxy "children")

authority ∷ RelDir Sandboxed
authority = dir_ (SProxy :: SProxy "security") </> dir_ (SProxy :: SProxy "authority")

licenseInfo ∷ RelFile Sandboxed
licenseInfo = dir_ (SProxy :: SProxy "server") </> file_ (SProxy :: SProxy "licenseInfo")

licensee ∷ RelFile Sandboxed
licensee = dir_ (SProxy :: SProxy "server") </> file_ (SProxy :: SProxy "licensee")

pdfInfo ∷ RelFile Sandboxed
pdfInfo = dir_ (SProxy :: SProxy "service") </> dir_ (SProxy :: SProxy "pdf") </> file_ (SProxy :: SProxy "info")

generatePdf ∷ RelFile Sandboxed
generatePdf = dir_ (SProxy :: SProxy "service") </> dir_ (SProxy :: SProxy "pdf") </> file_ (SProxy :: SProxy "generate")


