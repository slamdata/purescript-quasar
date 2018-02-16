module Quasar.Internal where

import Data.Path.Pathy (RelDir, RelFile, Sandboxed, file, dir)
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Data.Boolean (False)
import Type.Data.Symbol (class Equals)
import Unsafe.Coerce (unsafeCoerce)

class IsSymbolNonEmpty sym where
  reflectNonEmpty :: SProxy sym -> NonEmptyString

instance isSymbolNonEmpty :: (IsSymbol s, Equals s "" False) => IsSymbolNonEmpty s where
  reflectNonEmpty _ = asNonEmpty (reflectSymbol (SProxy :: SProxy s))
    where
    asNonEmpty :: String -> NonEmptyString
    asNonEmpty = unsafeCoerce


file_ :: forall s. IsSymbolNonEmpty s => SProxy s → RelFile Sandboxed
file_ x = file (reflectNonEmpty x)

dir_ :: forall s. IsSymbolNonEmpty s => SProxy s → RelDir Sandboxed
dir_ x = dir (reflectNonEmpty x)
