{-|
Module      : System
Description : Ivory wrappers for System API in Espressif SDK
Copyright   : (c) Chris Hodapp, 2016
License     : 
Maintainer  : hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

Unless otherwise specified, every Ivory procedure given here follows
its identically-named C function in the ESP8266 Non-OS SDK API Guide,
section 3.3 ("System APIs").

-}

{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Ivory.Language.ESP.System where

import Ivory.Language.IvoryUtil

import Ivory.Language
import Ivory.Language.Type

-- | Top-level Ivory module for System API (section 3.3 of non-OS API
-- docs) in Espressif SDK
system :: Module
system = package "system" $ do
  incl system_get_sdk_version
  incl system_restore
  incl system_restart
  incl system_init_done_cb
  incl system_get_chip_id
  incl system_get_vdd33
  incl system_adc_read
  incl system_deep_sleep
  incl system_deep_sleep_set_option
  incl system_phy_set_rfoption
  incl system_phy_set_powerup_option
  incl system_phy_set_max_tpw
  incl system_phy_set_tpw_via_vdd33
  incl system_set_os_print
  incl system_print_meminfo
  incl system_get_free_heap_size
  incl system_os_task
  incl system_os_post
  incl system_get_time
  incl system_get_rtc_time
  incl system_rtc_clock_cali_proc
  incl system_rtc_mem_write
  incl system_rtc_mem_read
  incl system_uart_swap
  incl system_uart_de_swap
  incl system_get_boot_version
  incl system_get_userbin_addr
  incl system_get_boot_mode
  incl system_restart_enhance
  incl system_update_cpu_freq
  incl system_get_cpu_freq
  incl system_get_flash_size_map
  incl system_get_rst_info
  incl system_soft_wdt_stop
  incl system_soft_wdt_restart
  incl system_soft_wdt_feed
  incl system_show_malloc
  incl os_memset
  incl os_memcpy
  incl os_strlen
  incl os_printf0
  incl os_bzero
  incl os_delay_us
  incl os_install_putc1

-- FIXME: What should foo.h be?

system_get_sdk_version :: Def('[] ':-> IString)
system_get_sdk_version = importProc "system_get_sdk_version" "foo.h"

system_restore :: Def('[] ':-> ())
system_restore = importProc "system_restore" "foo.h"

system_restart :: Def('[] ':-> ())
system_restart = importProc "system_restart" "foo.h"

system_init_done_cb :: Def('[ProcPtr ('[] ':-> ())] ':-> ())
system_init_done_cb = importProc "system_init_done_cb" "foo.h"

system_get_chip_id :: Def('[] ':-> Uint32)
system_get_chip_id = importProc "system_get_chip_id" "foo.h"

system_get_vdd33 :: Def('[] ':-> Uint16)
system_get_vdd33 = importProc "system_get_vdd33" "foo.h"

system_adc_read :: Def('[] ':-> Uint16)
system_adc_read = importProc "system_adc_read" "foo.h"

system_deep_sleep :: Def('[Uint32] ':-> ())
system_deep_sleep = importProc "system_deep_sleep" "foo.h"

system_deep_sleep_set_option :: Def('[Uint8] ':-> IBool)
system_deep_sleep_set_option = importProc "system_deep_sleep_set_option" "foo.h"

system_phy_set_rfoption :: Def('[Uint8] ':-> ())
system_phy_set_rfoption = importProc "system_phy_set_rfoption" "foo.h"

system_phy_set_powerup_option :: Def('[Uint8] ':-> ())
system_phy_set_powerup_option = importProc "system_phy_set_powerup_option" "foo.h"

system_phy_set_max_tpw :: Def('[Uint8] ':-> ())
system_phy_set_max_tpw = importProc "system_phy_set_max_tpw" "foo.h"

system_phy_set_tpw_via_vdd33 :: Def('[Uint16] ':-> ())
system_phy_set_tpw_via_vdd33 = importProc "system_phy_set_tpw_via_vdd33" "foo.h"

system_set_os_print :: Def('[Uint8] ':-> ())
system_set_os_print = importProc "system_set_os_print" "foo.h"

system_print_meminfo :: Def('[] ':-> ())
system_print_meminfo = importProc "system_print_meminfo" "foo.h"

system_get_free_heap_size :: Def('[] ':-> Uint32)
system_get_free_heap_size = importProc "system_get_free_heap_size" "foo.h"

-- FIXME: Need os_task_t, os_event_t, os_signal_t, os_param_t

system_os_task :: Def('[] ':-> ())  -- FIXME
system_os_task = importProc "system_os_task" "foo.h"

system_os_post :: Def('[] ':-> ())  -- FIXME
system_os_post = importProc "system_os_post" "foo.h"

system_get_time :: Def('[] ':-> Uint32)
system_get_time = importProc "system_get_time" "foo.h"

system_get_rtc_time :: Def('[] ':-> Uint32)
system_get_rtc_time = importProc "system_get_rtc_time" "foo.h"

system_rtc_clock_cali_proc :: Def('[] ':-> Uint32)
system_rtc_clock_cali_proc = importProc "system_rtc_clock_cali_proc" "foo.h"

-- FIXME: I'm not sure how to render the void* here.
system_rtc_mem_write :: Def('[Uint32, CBytes s, Uint32] ':-> IBool)
system_rtc_mem_write = importProc "system_rtc_mem_write" "foo.h"

-- FIXME: I'm again not sure how to render the void* here.
system_rtc_mem_read :: Def('[Uint32, CBytes s, Uint32] ':-> IBool)
system_rtc_mem_read = importProc "system_rtc_mem_read" "foo.h"

system_uart_swap :: Def('[] ':-> ())
system_uart_swap = importProc "system_uart_swap" "foo.h"

system_uart_de_swap :: Def('[] ':-> ())
system_uart_de_swap = importProc "system_uart_de_swap" "foo.h"

system_get_boot_version :: Def('[] ':-> Uint8)
system_get_boot_version = importProc "system_get_boot_version" "foo.h"

-- FIXME: I sort of doubt that Ivory will take well to turning a
-- Uint32 into a Ref/Ptr, so this might need some sort of less-kosher
-- C wrapper.
system_get_userbin_addr :: Def('[] ':-> Uint32)
system_get_userbin_addr = importProc "system_get_userbin_addr" "foo.h"

system_get_boot_mode :: Def('[] ':-> Uint8)
system_get_boot_mode = importProc "system_get_boot_mode" "foo.h"

system_restart_enhance :: Def('[Uint8, Uint32] ':-> IBool)
system_restart_enhance = importProc "system_restart_enhance" "foo.h"

system_update_cpu_freq :: Def('[Uint8] ':-> IBool)
system_update_cpu_freq = importProc "system_update_cpu_freq" "foo.h"

system_get_cpu_freq :: Def('[] ':-> Uint8)
system_get_cpu_freq = importProc "system_get_cpu_freq" "foo.h"

-- FIXME: Need to handle flash_size_map enum.
system_get_flash_size_map :: Def('[] ':-> Uint32)  -- FIXME
system_get_flash_size_map = importProc "system_get_flash_size_map" "foo.h"

-- FIXME: Need to handle rst_reason enum & rst_info struct.
system_get_rst_info :: Def('[] ':-> ())  -- FIXME
system_get_rst_info = importProc "system_get_rst_info" "foo.h"

system_soft_wdt_stop :: Def('[] ':-> ())
system_soft_wdt_stop = importProc "system_soft_wdt_stop" "foo.h"

system_soft_wdt_restart :: Def('[] ':-> ())
system_soft_wdt_restart = importProc "system_soft_wdt_restart" "foo.h"

system_soft_wdt_feed :: Def('[] ':-> ())
system_soft_wdt_feed = importProc "system_soft_wdt_feed" "foo.h"

system_show_malloc :: Def('[] ':-> ())
system_show_malloc = importProc "system_show_malloc" "foo.h"

-- FIXME: Is Uint8 okay for 'int', and Uint32 okay for size_t?
os_memset :: Def('[CBytes s, Uint8, Uint32] ':-> ())
os_memset = importProc "os_memset" "foo.h"

-- FIXME: Is Uint32 okay for size_t?  Should source or destination be
-- made into Ref or Ptr more generally?
os_memcpy :: Def('[CBytes s, CBytes t, Uint32] ':-> ())
os_memcpy = importProc "os_memcpy" "foo.h"

-- FIXME: Docs don't say return type, just that it's a size.
os_strlen :: Def('[IString] ':-> Uint32)
os_strlen = importProc "os_strlen" "foo.h"

-- | Variadic functions don't really seem to be possible in any
-- meaningful sense in Ivory, so printf is specified by the number of
-- other arguments it takes (in addition to the string).  This version
-- takes none.
os_printf0 :: () => Def ('[IString] ':-> Sint32)
os_printf0 = importProc "printf" "foo.h"

-- | 'os_printf' with 1 argument
os_printf1 :: (IvoryType s1) => Def ('[IString, s1] ':-> Sint32)
os_printf1 = importProc "printf" "foo.h"

-- | 'os_printf' with 2 arguments
os_printf2 :: (IvoryType s1, IvoryType s2) =>
              Def ('[IString, s1, s2] ':-> Sint32)
os_printf2 = importProc "printf" "foo.h"

-- | 'os_printf' with 3 arguments
os_printf3 :: (IvoryType s1, IvoryType s2, IvoryType s3) =>
              Def ('[IString, s1, s2, s3] ':-> Sint32)
os_printf3 = importProc "printf" "foo.h"

-- | 'os_printf' with 4 arguments
os_printf4 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4) =>
              Def ('[IString, s1, s2, s3, s4] ':-> Sint32)
os_printf4 = importProc "printf" "foo.h"

-- | 'os_printf' with 5 arguments
os_printf5 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
               IvoryType s5) =>
              Def ('[IString, s1, s2, s3, s4, s5] ':-> Sint32)
os_printf5 = importProc "printf" "foo.h"

-- | 'os_printf' with 6 arguments
os_printf6 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
               IvoryType s5, IvoryType s6) =>
              Def ('[IString, s1, s2, s3, s4, s5, s6] ':-> Sint32)
os_printf6 = importProc "printf" "foo.h"

-- | 'os_printf' with 7 arguments
os_printf7 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
               IvoryType s5, IvoryType s6, IvoryType s7) =>
              Def ('[IString, s1, s2, s3, s4, s5, s6, s7] ':-> Sint32)
os_printf7 = importProc "printf" "foo.h"

-- | 'os_printf' with 8 arguments
os_printf8 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
               IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8) =>
              Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8] ':-> Sint32)
os_printf8 = importProc "printf" "foo.h"

-- | 'os_printf' with 9 arguments
os_printf9 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
               IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
               IvoryType s9) =>
              Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9] ':-> Sint32)
os_printf9 = importProc "printf" "foo.h"

-- | 'os_printf' with 10 arguments
os_printf10 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
                IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
                IvoryType s9, IvoryType s10) =>
               Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10] ':->
                    Sint32)
os_printf10 = importProc "printf" "foo.h"

-- | 'os_printf' with 11 arguments
os_printf11 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
                IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
                IvoryType s9, IvoryType s10, IvoryType s11) =>
               Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11]
                    ':-> Sint32)
os_printf11 = importProc "printf" "foo.h"

-- | 'os_printf' with 12 arguments
os_printf12 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
                IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
                IvoryType s9, IvoryType s10, IvoryType s11, IvoryType s12) =>
               Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
                      s12] ':-> Sint32)
os_printf12 = importProc "printf" "foo.h"

-- | 'os_printf' with 13 arguments
os_printf13 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
                IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
                IvoryType s9, IvoryType s10, IvoryType s11, IvoryType s12,
                IvoryType s13) =>
               Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
                      s12, s13] ':-> Sint32)
os_printf13 = importProc "printf" "foo.h"

-- | 'os_printf' with 14 arguments
os_printf14 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
                IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
                IvoryType s9, IvoryType s10, IvoryType s11, IvoryType s12,
                IvoryType s13, IvoryType s14) =>
               Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
                      s12, s13, s14] ':-> Sint32)
os_printf14 = importProc "printf" "foo.h"

-- | 'os_printf' with 15 arguments
os_printf15 :: (IvoryType s1, IvoryType s2, IvoryType s3, IvoryType s4,
                IvoryType s5, IvoryType s6, IvoryType s7, IvoryType s8,
                IvoryType s9, IvoryType s10, IvoryType s11, IvoryType s12,
                IvoryType s13, IvoryType s14, IvoryType s15) =>
               Def ('[IString, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
                      s12, s13, s14, s15] ':-> Sint32)
os_printf15 = importProc "printf" "foo.h"

-- FIXME: Not sure if either of these types are the best ones
os_bzero :: Def('[CBytes s, Uint32] ':-> ())
os_bzero = importProc "os_bzero" "foo.h"

os_delay_us :: Def('[Uint16] ':-> ())
os_delay_us = importProc "os_delay_us" "foo.h"

os_install_putc1 :: Def('[ProcPtr ('[IChar] ':-> ())] ':-> ())
os_install_putc1 = importProc "os_install_putc1" "foo.h"
