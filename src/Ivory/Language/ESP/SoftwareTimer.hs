{-|
Module      : SoftwareTimer
Description : Ivory wrappers for software timer API in Espressif SDK
Copyright   : (c) Chris Hodapp, 2016
License     : 
Maintainer  : hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DataKinds, TypeOperators #-}

module Ivory.Language.ESP.SoftwareTimer where

import Ivory.Language

-- | Top-level Ivory module for software timers in Espressif SDK
swtimer :: Module
swtimer = package "swtimer" $ do
  incl os_timer_arm
  incl os_timer_disarm
  incl os_timer_setfn
  incl system_timer_reinit
  incl os_timer_arm_us

-- | Equivalent to 'os_timer_t *'
type OsTimerRef = Ptr Global (Stored ())
-- FIXME: This should be os_timer_t in some fashion, not just a void*.
-- It should really just be an opaque pointer since we should not
-- expose ETSTimer, even if that's the underlying type.

-- | Equivalent to 'os_timer_func_t'
type OsTimerFunc = '[Ptr Global (Stored ())] :-> ()

os_timer_arm :: Def('[OsTimerRef, Uint32, IBool] :-> ())
os_timer_arm = importProc "os_timer_arm" "osapi.h"

os_timer_disarm :: Def('[OsTimerRef] :-> ())
os_timer_disarm = importProc "os_timer_disarm" "osapi.h"

os_timer_setfn ::
  Def('[OsTimerRef, ProcPtr OsTimerFunc, Ptr Global (Stored ())] :-> ())
os_timer_setfn = importProc "os_timer_setfn" "osapi.h"

system_timer_reinit :: Def('[] :-> ())
system_timer_reinit = importProc "system_timer_reinit" "osapi.h"

os_timer_arm_us :: Def('[OsTimerRef, Uint32, IBool] :-> ())
os_timer_arm_us = importProc "os_timer_arm_us" "osapi.h"
