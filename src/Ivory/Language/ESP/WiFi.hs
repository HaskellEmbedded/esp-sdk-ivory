{-|
Module      : WiFi
Description : Ivory wrappers for Wi-Fi API in Espressif SDK
Copyright   : (c) Chris Hodapp, 2016
License     : 
Maintainer  : hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

Unless otherwise specified, every Ivory procedure given here follows
its identically-named C function in the ESP8266 Non-OS SDK API Guide,
section 3.5 ("Wi-Fi Flash Related APIs").

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.ESP.WiFi where

import Ivory.Language.IvoryUtil

import Ivory.Language
import Ivory.Language.Type

import qualified Ivory.Language.Syntax.Concrete.ParseAST as PA
import qualified Ivory.Language.Syntax.Concrete.Location as L
import qualified Ivory.Language.Syntax.Concrete.QQ.StructQQ as SQQ

-- FIXME: Get array types into this structure
SQQ.fromStruct $ PA.StructDef "station_config"
  [ PA.Field "ssid"      (PA.TyWord PA.Word32) L.NoLoc -- uint8[32]
  , PA.Field "password"  (PA.TyWord PA.Word32) L.NoLoc -- uint8[64]
  , PA.Field "bssid_set" (PA.TyWord PA.Word8)  L.NoLoc
  , PA.Field "bssid"     (PA.TyWord PA.Word32) L.NoLoc -- uint8[6]
  ] L.NoLoc

-- FIXME: Get pointer types into this structure
--
-- FIXME: I can't use two 'ssid' or 'bssid' fields in the same file,
-- though C permits this.  However, I can't change it here without
-- breaking compatibility.
SQQ.fromStruct $ PA.StructDef "scan_config"
  [ PA.Field "ssid_fixme"  (PA.TyWord PA.Word8) L.NoLoc -- uint8*
  , PA.Field "bssid_fixme" (PA.TyWord PA.Word8) L.NoLoc -- uint8*
  , PA.Field "channel"     (PA.TyWord PA.Word8) L.NoLoc
  , PA.Field "show_hidden" (PA.TyWord PA.Word8) L.NoLoc
  ] L.NoLoc

-- | Equivalent to @struct station_config@
type StationConfig = Struct "station_config"

-- | Equivalent to @struct scan_config@
type ScanConfig = Struct "scan_config"

wifi :: Module
wifi = package "wifi" $ do
  defStruct (Proxy :: Proxy "station_config")
  defStruct (Proxy :: Proxy "scan_config")
  incl wifi_get_opmode
  incl wifi_get_opmode_default
  incl wifi_set_opmode
  incl wifi_set_opmode_current
  incl wifi_station_get_config
  incl wifi_station_get_config_default
  incl wifi_station_set_config
  incl wifi_station_set_config_current
  incl wifi_station_set_cert_key
  incl wifi_station_clear_cert_key
  incl wifi_station_set_username
  incl wifi_station_clear_username
  incl wifi_station_connect
  incl wifi_station_disconnect
  incl wifi_station_get_connect_status
  incl wifi_station_scan
  incl scan_done_cb_t
  incl wifi_station_ap_number_set
  incl wifi_station_get_ap_info
  incl wifi_station_ap_change
  incl wifi_station_get_current_ap_id
  incl wifi_station_get_auto_connect
  incl wifi_station_set_auto_connect
  incl wifi_station_dhcpc_start
  incl wifi_station_dhcpc_stop
  incl wifi_station_dhcpc_status
  incl wifi_station_dhcpc_set_maxtry
  incl wifi_station_set_reconnect_policy
  incl wifi_station_get_rssi
  incl wifi_station_set_hostname
  incl wifi_station_get_hostname
  incl wifi_softap_get_config
  incl wifi_softap_get_config_default
  incl wifi_softap_set_config
  incl wifi_softap_set_config_current
  incl wifi_softap_get_station_num
  incl wifi_softap_get_station_info
  incl wifi_softap_free_station_info
  incl wifi_softap_dhcps_start
  incl wifi_softap_dhcps_stop
  incl wifi_softap_set_dhcps_lease
  incl wifi_softap_get_dhcps_lease
  incl wifi_softap_set_dhcps_lease_time
  incl wifi_softap_get_dhcps_lease_time
  incl wifi_softap_reset_dhcps_lease_time
  incl wifi_softap_dhcps_status
  incl wifi_softap_set_dhcps_offer_option
  incl wifi_set_phy_mode
  incl wifi_get_phy_mode
  incl wifi_get_ip_info
  incl wifi_set_ip_info
  incl wifi_set_macaddr
  incl wifi_get_macaddr
  incl wifi_set_sleep_type
  incl wifi_get_sleep_type
  incl wifi_status_led_install
  incl wifi_status_led_uninstall
  incl wifi_set_broadcast_if
  incl wifi_get_broadcast_if
  incl wifi_set_event_handler_cb
  incl wifi_wps_enable
  incl wifi_wps_disable
  incl wifi_wps_start
  incl wifi_set_wps_cb
  incl wifi_register_send_pkt_freedom_cb
  incl wifi_unregister_send_pkt_freedom_cb
  incl wifi_send_pkt_freedom
  incl wifi_rfid_locp_recv_open
  incl wifi_rfid_locp_recv_close
  incl wifi_register_rfid_locp_recv_cb
  incl wifi_unregister_rfid_locp_recv_cb

-- | Constant for station mode
mode_station :: Uint8
mode_station = 0x01

-- | Constant for soft-AP mode
mode_soft_ap :: Uint8
mode_soft_ap = 0x02

-- | Constant for station+soft-AP mode
mode_station_and_soft_ap :: Uint8
mode_station_and_soft_ap = 0x03

-- | Note that this should return 'mode_station', 'mode_soft_ap', or
-- 'mode_station_and_soft_ap'.
wifi_get_opmode :: Def('[] ':-> Uint8)
wifi_get_opmode = importProc "wifi_get_opmode" "user_interface.h"

wifi_get_opmode_default :: Def('[] ':-> Uint8)
wifi_get_opmode_default = importProc "wifi_get_opmode_default" "user_interface.h"

wifi_set_opmode :: Def('[Uint8] ':-> IBool)
wifi_set_opmode = importProc "wifi_set_opmode" "user_interface.h"

wifi_set_opmode_current :: Def('[Uint8] ':-> IBool)
wifi_set_opmode_current = importProc "wifi_set_opmode_current" "user_interface.h"

wifi_station_get_config :: Def('[Ref s StationConfig] ':-> IBool)
wifi_station_get_config = importProc "wifi_station_get_config" "user_interface.h"

wifi_station_get_config_default :: Def('[Ref s StationConfig] ':-> IBool)
wifi_station_get_config_default = importProc "wifi_station_get_config_default" "user_interface.h"

wifi_station_set_config :: Def('[Ref s StationConfig] ':-> IBool)
wifi_station_set_config = importProc "wifi_station_set_config" "user_interface.h"

wifi_station_set_config_current :: Def('[Ref s StationConfig] ':-> IBool)
wifi_station_set_config_current = importProc "wifi_station_set_config_current" "user_interface.h"

-- FIXME Is this the easiest way to do this?
wifi_station_set_cert_key :: Def('[CBytes s, Sint32, CBytes t, Sint32, CBytes u, Sint32] ':-> IBool)
wifi_station_set_cert_key = importProc "wifi_station_set_cert_key" "user_interface.h"

wifi_station_clear_cert_key :: Def('[] ':-> ())
wifi_station_clear_cert_key = importProc "wifi_station_clear_cert_key" "user_interface.h"

wifi_station_set_username :: Def('[CBytes s, Sint32] ':-> Sint32)
wifi_station_set_username = importProc "wifi_station_set_username" "user_interface.h"

wifi_station_clear_username :: Def('[] ':-> ())
wifi_station_clear_username = importProc "wifi_station_clear_username" "user_interface.h"

wifi_station_connect :: Def('[] ':-> IBool)
wifi_station_connect = importProc "wifi_station_connect" "user_interface.h"

wifi_station_disconnect :: Def('[] ':-> IBool)
wifi_station_disconnect = importProc "wifi_station_disconnect" "user_interface.h"

-- | Equivalent to @STATION_IDLE@
station_idle :: Uint8
station_idle = extern "STATION_IDLE" "user_interface.h"

-- | Equivalent to @STATION_CONNECTING@
station_connecting :: Uint8
station_connecting = extern "STATION_CONNECTING" "user_interface.h"

-- | Equivalent to @STATION_WRONG_PASSWORD@
station_wrong_password :: Uint8
station_wrong_password = extern "STATION_WRONG_PASSWORD" "user_interface.h"

-- | Equivalent to @STATION_NO_AP_FOUND@
station_no_ap_found :: Uint8
station_no_ap_found = extern "STATION_NO_AP_FOUND" "user_interface.h"

-- | Equivalent to @STATION_CONNECT_FAIL@
station_connect_fail :: Uint8
station_connect_fail = extern "STATION_CONNECT_FAIL" "user_interface.h"

-- | Equivalent to @STATION_GOT_IP@
station_got_ip :: Uint8
station_got_ip = extern "STATION_GOT_IP" "user_interface.h"

-- | See @STATION_*@ constants for the return values of this
wifi_station_get_connect_status :: Def('[] ':-> Uint8)
wifi_station_get_connect_status = importProc "wifi_station_get_connect_status" "user_interface.h"

-- | Equivalent to @STATUS@ enum
type Status = Uint32

-- | Equivalent to @OK@ of @STATUS@ enum
status_OK :: Status
status_OK = extern "OK" "c_types.h"

-- | Equivalent to @FAIL@ of @STATUS@ enum
status_FAIL :: Status
status_FAIL = extern "FAIL" "c_types.h"

-- | Equivalent to @PENDING@ of @STATUS@ enum
status_PENDING :: Status
status_PENDING = extern "PENDING" "c_types.h"

-- | Equivalent to @BUSY@ of @STATUS@ enum
status_BUSY :: Status
status_BUSY = extern "BUSY" "c_types.h"

-- | Equivalent to @CANCEL@ of @STATUS@ enum
status_CANCEL :: Status
status_CANCEL = extern "CANCEL" "c_types.h"

-- | Equivalent to 'scan_done_cb_t'
type ScanDoneCbT s = '[Ptr s (Stored ()), Status] :-> ()
-- FIXME: I'm not sure if that's the best way to express a void*

wifi_station_scan :: Def('[Ref s ScanConfig, ProcPtr (ScanDoneCbT t)] ':-> IBool)
wifi_station_scan = importProc "wifi_station_scan" "user_interface.h"

-- FIXME (auto-generated)
scan_done_cb_t :: Def('[] ':-> ())
scan_done_cb_t = importProc "scan_done_cb_t" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_ap_number_set :: Def('[] ':-> ())
wifi_station_ap_number_set = importProc "wifi_station_ap_number_set" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_get_ap_info :: Def('[] ':-> ())
wifi_station_get_ap_info = importProc "wifi_station_get_ap_info" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_ap_change :: Def('[] ':-> ())
wifi_station_ap_change = importProc "wifi_station_ap_change" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_get_current_ap_id :: Def('[] ':-> ())
wifi_station_get_current_ap_id = importProc "wifi_station_get_current_ap_id" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_get_auto_connect :: Def('[] ':-> ())
wifi_station_get_auto_connect = importProc "wifi_station_get_auto_connect" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_set_auto_connect :: Def('[] ':-> ())
wifi_station_set_auto_connect = importProc "wifi_station_set_auto_connect" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_dhcpc_start :: Def('[] ':-> ())
wifi_station_dhcpc_start = importProc "wifi_station_dhcpc_start" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_dhcpc_stop :: Def('[] ':-> ())
wifi_station_dhcpc_stop = importProc "wifi_station_dhcpc_stop" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_dhcpc_status :: Def('[] ':-> ())
wifi_station_dhcpc_status = importProc "wifi_station_dhcpc_status" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_dhcpc_set_maxtry :: Def('[] ':-> ())
wifi_station_dhcpc_set_maxtry = importProc "wifi_station_dhcpc_set_maxtry" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_set_reconnect_policy :: Def('[] ':-> ())
wifi_station_set_reconnect_policy = importProc "wifi_station_set_reconnect_policy" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_get_rssi :: Def('[] ':-> ())
wifi_station_get_rssi = importProc "wifi_station_get_rssi" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_set_hostname :: Def('[] ':-> ())
wifi_station_set_hostname = importProc "wifi_station_set_hostname" "user_interface.h"

-- FIXME (auto-generated)
wifi_station_get_hostname :: Def('[] ':-> ())
wifi_station_get_hostname = importProc "wifi_station_get_hostname" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_config :: Def('[] ':-> ())
wifi_softap_get_config = importProc "wifi_softap_get_config" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_config_default :: Def('[] ':-> ())
wifi_softap_get_config_default = importProc "wifi_softap_get_config_default" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_set_config :: Def('[] ':-> ())
wifi_softap_set_config = importProc "wifi_softap_set_config" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_set_config_current :: Def('[] ':-> ())
wifi_softap_set_config_current = importProc "wifi_softap_set_config_current" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_station_num :: Def('[] ':-> ())
wifi_softap_get_station_num = importProc "wifi_softap_get_station_num" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_station_info :: Def('[] ':-> ())
wifi_softap_get_station_info = importProc "wifi_softap_get_station_info" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_free_station_info :: Def('[] ':-> ())
wifi_softap_free_station_info = importProc "wifi_softap_free_station_info" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_dhcps_start :: Def('[] ':-> ())
wifi_softap_dhcps_start = importProc "wifi_softap_dhcps_start" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_dhcps_stop :: Def('[] ':-> ())
wifi_softap_dhcps_stop = importProc "wifi_softap_dhcps_stop" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_set_dhcps_lease :: Def('[] ':-> ())
wifi_softap_set_dhcps_lease = importProc "wifi_softap_set_dhcps_lease" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_dhcps_lease :: Def('[] ':-> ())
wifi_softap_get_dhcps_lease = importProc "wifi_softap_get_dhcps_lease" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_set_dhcps_lease_time :: Def('[] ':-> ())
wifi_softap_set_dhcps_lease_time = importProc "wifi_softap_set_dhcps_lease_time" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_dhcps_lease_time :: Def('[] ':-> ())
wifi_softap_get_dhcps_lease_time = importProc "wifi_softap_get_dhcps_lease_time" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_reset_dhcps_lease_time :: Def('[] ':-> ())
wifi_softap_reset_dhcps_lease_time = importProc "wifi_softap_reset_dhcps_lease_time" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_dhcps_status :: Def('[] ':-> ())
wifi_softap_dhcps_status = importProc "wifi_softap_dhcps_status" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_set_dhcps_offer_option :: Def('[] ':-> ())
wifi_softap_set_dhcps_offer_option = importProc "wifi_softap_set_dhcps_offer_option" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_phy_mode :: Def('[] ':-> ())
wifi_set_phy_mode = importProc "wifi_set_phy_mode" "user_interface.h"

-- FIXME (auto-generated)
wifi_get_phy_mode :: Def('[] ':-> ())
wifi_get_phy_mode = importProc "wifi_get_phy_mode" "user_interface.h"

-- FIXME (auto-generated)
wifi_get_ip_info :: Def('[] ':-> ())
wifi_get_ip_info = importProc "wifi_get_ip_info" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_ip_info :: Def('[] ':-> ())
wifi_set_ip_info = importProc "wifi_set_ip_info" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_macaddr :: Def('[] ':-> ())
wifi_set_macaddr = importProc "wifi_set_macaddr" "user_interface.h"

-- FIXME (auto-generated)
wifi_get_macaddr :: Def('[] ':-> ())
wifi_get_macaddr = importProc "wifi_get_macaddr" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_sleep_type :: Def('[] ':-> ())
wifi_set_sleep_type = importProc "wifi_set_sleep_type" "user_interface.h"

-- FIXME (auto-generated)
wifi_get_sleep_type :: Def('[] ':-> ())
wifi_get_sleep_type = importProc "wifi_get_sleep_type" "user_interface.h"

-- FIXME (auto-generated)
wifi_status_led_install :: Def('[] ':-> ())
wifi_status_led_install = importProc "wifi_status_led_install" "user_interface.h"

-- FIXME (auto-generated)
wifi_status_led_uninstall :: Def('[] ':-> ())
wifi_status_led_uninstall = importProc "wifi_status_led_uninstall" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_broadcast_if :: Def('[] ':-> ())
wifi_set_broadcast_if = importProc "wifi_set_broadcast_if" "user_interface.h"

-- FIXME (auto-generated)
wifi_get_broadcast_if :: Def('[] ':-> ())
wifi_get_broadcast_if = importProc "wifi_get_broadcast_if" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_event_handler_cb :: Def('[] ':-> ())
wifi_set_event_handler_cb = importProc "wifi_set_event_handler_cb" "user_interface.h"

-- FIXME (auto-generated)
wifi_wps_enable :: Def('[] ':-> ())
wifi_wps_enable = importProc "wifi_wps_enable" "user_interface.h"

-- FIXME (auto-generated)
wifi_wps_disable :: Def('[] ':-> ())
wifi_wps_disable = importProc "wifi_wps_disable" "user_interface.h"

-- FIXME (auto-generated)
wifi_wps_start :: Def('[] ':-> ())
wifi_wps_start = importProc "wifi_wps_start" "user_interface.h"

-- FIXME (auto-generated)
wifi_set_wps_cb :: Def('[] ':-> ())
wifi_set_wps_cb = importProc "wifi_set_wps_cb" "user_interface.h"

-- FIXME (auto-generated)
wifi_register_send_pkt_freedom_cb :: Def('[] ':-> ())
wifi_register_send_pkt_freedom_cb = importProc "wifi_register_send_pkt_freedom_cb" "user_interface.h"

-- FIXME (auto-generated)
wifi_unregister_send_pkt_freedom_cb :: Def('[] ':-> ())
wifi_unregister_send_pkt_freedom_cb = importProc "wifi_unregister_send_pkt_freedom_cb" "user_interface.h"

-- FIXME (auto-generated)
wifi_send_pkt_freedom :: Def('[] ':-> ())
wifi_send_pkt_freedom = importProc "wifi_send_pkt_freedom" "user_interface.h"

-- FIXME (auto-generated)
wifi_rfid_locp_recv_open :: Def('[] ':-> ())
wifi_rfid_locp_recv_open = importProc "wifi_rfid_locp_recv_open" "user_interface.h"

-- FIXME (auto-generated)
wifi_rfid_locp_recv_close :: Def('[] ':-> ())
wifi_rfid_locp_recv_close = importProc "wifi_rfid_locp_recv_close" "user_interface.h"

-- FIXME (auto-generated)
wifi_register_rfid_locp_recv_cb :: Def('[] ':-> ())
wifi_register_rfid_locp_recv_cb = importProc "wifi_register_rfid_locp_recv_cb" "user_interface.h"

-- FIXME (auto-generated)
wifi_unregister_rfid_locp_recv_cb :: Def('[] ':-> ())
wifi_unregister_rfid_locp_recv_cb = importProc "wifi_unregister_rfid_locp_recv_cb" "user_interface.h"
