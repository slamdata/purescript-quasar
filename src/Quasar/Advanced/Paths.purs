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

import Pathy (RelDir, RelFile, dir, file, (</>))
import Data.Symbol (SProxy(..))

oidcProviders ∷ RelFile
oidcProviders = dir (SProxy ∷ SProxy "security") </> dir (SProxy ∷ SProxy "oidc") </> file (SProxy ∷ SProxy "providers")

token ∷ RelDir
token = dir (SProxy ∷ SProxy "security") </> dir (SProxy ∷ SProxy "token")

group ∷ RelDir
group = dir (SProxy ∷ SProxy "security") </> dir (SProxy ∷ SProxy "group")

permission ∷ RelDir
permission = dir (SProxy ∷ SProxy "security") </> dir (SProxy ∷ SProxy "permission")

children ∷ RelFile
children = file (SProxy ∷ SProxy "children")

authority ∷ RelDir
authority = dir (SProxy ∷ SProxy "security") </> dir (SProxy ∷ SProxy "authority")

licenseInfo ∷ RelFile
licenseInfo = dir (SProxy ∷ SProxy "server") </> file (SProxy ∷ SProxy "licenseInfo")

licensee ∷ RelFile
licensee = dir (SProxy ∷ SProxy "server") </> file (SProxy ∷ SProxy "licensee")

pdfInfo ∷ RelFile
pdfInfo = dir (SProxy ∷ SProxy "service") </> dir (SProxy ∷ SProxy "pdf") </> file (SProxy ∷ SProxy "info")

generatePdf ∷ RelFile
generatePdf = dir (SProxy ∷ SProxy "service") </> dir (SProxy ∷ SProxy "pdf") </> file (SProxy ∷ SProxy "generate")
