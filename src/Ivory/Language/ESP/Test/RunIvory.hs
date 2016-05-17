{-# LANGUAGE DataKinds, TypeOperators #-}

module Main where

import Ivory.Language.ESP.SoftwareTimer
import Ivory.Language.ESP.System

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

main :: IO ()
main = do
  -- FIXME: Make this path something sane.  If I make it Nothing, then
  -- it's anybody's guess where the files end up.
  let outDir = Nothing
      modules = [swtimer, system, test]
  runCompiler modules [] initialOpts { outDir = outDir }

test :: Module
test = package "test" $ do
  depend swtimer
  depend system
  incl test_arm_timer

test_arm_timer :: Def('[] ':-> ())
test_arm_timer = proc "test_arm_timer" $ body $ do
  timer <- local (istruct [] :: Init OsTimerT)
  call_ os_timer_arm timer 0 false
