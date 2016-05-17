{-|
Module      : SoftwareTimer
Description : Ivory wrappers for software timer API in Espressif SDK
Copyright   : (c) Chris Hodapp, 2016
License     : 
Maintainer  : hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

Unless otherwise specified, every Ivory procedure given here follows
its identically-named C function in the ESP8266 Non-OS SDK API Guide,
section 3.1 ("Software Timer").

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.ESP.SoftwareTimer where

import Ivory.Language

import qualified Ivory.Language.Syntax.Concrete.ParseAST as PA
import qualified Ivory.Language.Syntax.Concrete.Location as L
import qualified Ivory.Language.Syntax.Concrete.QQ.StructQQ as SQQ

-- I sort of don't feel like using quasiquoting, but it's an option
-- (replace TemplateHaskell with QuasiQuotes if using this):
-- [ivory|
--     abstract struct os_timer_t "osapi.h"
-- |]

-- Instead I sort of prefer this:
SQQ.fromStruct $ PA.AbstractDef "os_timer_t" "osapi.h" L.NoLoc
-- FIXME: The above generates 'struct os_timer_t' but I only need
-- 'os_timer_t' as it's just an opaque type.
-- https://github.com/GaloisInc/ivory/issues/6
-- https://github.com/GaloisInc/ivory/issues/62

type OsTimerT = Struct "os_timer_t"

-- | Top-level Ivory module for software timers (section 3.1 of non-OS
-- API) in Espressif SDK
swtimer :: Module
swtimer = package "swtimer" $ do
  incl os_timer_arm
  incl os_timer_disarm
  incl os_timer_setfn
  incl system_timer_reinit
  incl os_timer_arm_us
  defStruct (Proxy :: Proxy "os_timer_t")

-- | Equivalent to 'os_timer_t *'
type OsTimerRef s = Ref s OsTimerT

-- | Equivalent to 'os_timer_func_t'
type OsTimerFunc = '[Ptr Global (Stored ())] :-> ()

os_timer_arm :: Def('[OsTimerRef s, Uint32, IBool] :-> ())
os_timer_arm = importProc "os_timer_arm" "osapi.h"

os_timer_disarm :: Def('[OsTimerRef s] :-> ())
os_timer_disarm = importProc "os_timer_disarm" "osapi.h"

os_timer_setfn ::
  Def('[OsTimerRef s, ProcPtr OsTimerFunc, Ptr Global (Stored ())] :-> ())
os_timer_setfn = importProc "os_timer_setfn" "osapi.h"

system_timer_reinit :: Def('[] :-> ())
system_timer_reinit = importProc "system_timer_reinit" "osapi.h"

os_timer_arm_us :: Def('[OsTimerRef s, Uint32, IBool] :-> ())
os_timer_arm_us = importProc "os_timer_arm_us" "osapi.h"
