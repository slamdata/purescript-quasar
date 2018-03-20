module Test.Util.Effect where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)

import Network.HTTP.Affjax (AJAX)

import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)

import Test.Assert (ASSERT)

type Effects =
  ( avar ∷ AVAR
  , cp ∷ CHILD_PROCESS
  , process ∷ PROCESS
  , exception ∷ EXCEPTION
  , fs ∷ FS
  , buffer ∷ BUFFER
  , console ∷ CONSOLE
  , ajax ∷ AJAX
  , assert ∷ ASSERT
  )
