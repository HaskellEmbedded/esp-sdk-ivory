{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts #-}

module Ivory.Language.ESP.SoftwareTimer where

import           Ivory.Language

cmodule :: Module
cmodule = package "swtimer" $ do
  -- incl whatever...
  -- inclSym...
  return ()

-- FIXME: Stored () should be os_timer_t in some fashion.
os_timer_arm :: Def('[Ptr Global (Stored ()), Uint32, IBool] :-> ())
os_timer_arm = importProc "os_timer_arm" "osapi.h"
