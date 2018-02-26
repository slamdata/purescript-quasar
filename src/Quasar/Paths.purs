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

module Quasar.Paths where

import Pathy (RelDir, RelFile, file, dir, (</>))
import Data.Symbol (SProxy(..))

upload ∷ RelFile
upload = file (SProxy :: SProxy "upload")

metadata ∷ RelDir
metadata = dir (SProxy :: SProxy "metadata") </> dir (SProxy :: SProxy "fs")

metastore ∷ RelFile
metastore = file (SProxy :: SProxy "metastore")

mount ∷ RelDir
mount = dir (SProxy :: SProxy "mount") </> dir (SProxy :: SProxy "fs")

data_ ∷ RelDir
data_ = dir (SProxy :: SProxy "data") </> dir (SProxy :: SProxy "fs")

query ∷ RelDir
query = dir (SProxy :: SProxy "query") </> dir (SProxy :: SProxy "fs")

compile ∷ RelDir
compile = dir (SProxy :: SProxy "compile") </> dir (SProxy :: SProxy "fs")

serverInfo ∷ RelFile
serverInfo = dir (SProxy :: SProxy "server") </> file (SProxy :: SProxy "info")

invoke ∷ RelDir
invoke = dir (SProxy :: SProxy "invoke") </> dir (SProxy :: SProxy "fs")
