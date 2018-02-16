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

import Data.Path.Pathy (RelDir, RelFile, Sandboxed, (</>))
import Data.Symbol (SProxy(..))
import Quasar.Internal (file_, dir_)

upload ∷ RelFile Sandboxed
upload = file_ (SProxy :: SProxy "upload")

metadata ∷ RelDir Sandboxed
metadata = dir_ (SProxy :: SProxy "metadata") </> dir_ (SProxy :: SProxy "fs")

metastore ∷ RelFile Sandboxed
metastore = file_ (SProxy :: SProxy "metastore")

mount ∷ RelDir Sandboxed
mount = dir_ (SProxy :: SProxy "mount") </> dir_ (SProxy :: SProxy "fs")

data_ ∷ RelDir Sandboxed
data_ = dir_ (SProxy :: SProxy "data") </> dir_ (SProxy :: SProxy "fs")

query ∷ RelDir Sandboxed
query = dir_ (SProxy :: SProxy "query") </> dir_ (SProxy :: SProxy "fs")

compile ∷ RelDir Sandboxed
compile = dir_ (SProxy :: SProxy "compile") </> dir_ (SProxy :: SProxy "fs")

serverInfo ∷ RelFile Sandboxed
serverInfo = dir_ (SProxy :: SProxy "server") </> file_ (SProxy :: SProxy "info")

invoke ∷ RelDir Sandboxed
invoke = dir_ (SProxy :: SProxy "invoke") </> dir_ (SProxy :: SProxy "fs")
