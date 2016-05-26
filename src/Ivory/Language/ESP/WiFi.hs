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

-- FIXME: Get array types into this structure as marked
SQQ.fromStruct $ PA.StructDef "station_config"
  [ PA.Field "ssid"      (PA.TyWord PA.Word32) L.NoLoc -- uint8[32]
  , PA.Field "password"  (PA.TyWord PA.Word32) L.NoLoc -- uint8[64]
  , PA.Field "bssid_set" (PA.TyWord PA.Word8)  L.NoLoc
  , PA.Field "bssid"     (PA.TyWord PA.Word32) L.NoLoc -- uint8[6]
  ] L.NoLoc

-- | Equivalent to @struct station_config@
type StationConfig = Struct "station_config"

-- FIXME: Get pointer types into this structure
--
-- FIXME: 'ssid_scan' should be 'ssid', 'bssid_scan' should be
-- 'bssid', but I can't use two 'ssid' or 'bssid' fields in the same
-- file, though C permits this.  However, I can't change it here
-- without breaking compatibility.
SQQ.fromStruct $ PA.StructDef "scan_config"
  [ PA.Field "ssid_scan"   (PA.TyWord PA.Word8) L.NoLoc -- uint8*
  , PA.Field "bssid_scan"  (PA.TyWord PA.Word8) L.NoLoc -- uint8*
  , PA.Field "channel"     (PA.TyWord PA.Word8) L.NoLoc
  , PA.Field "show_hidden" (PA.TyWord PA.Word8) L.NoLoc
  ] L.NoLoc

-- | Equivalent to @struct scan_config@
type ScanConfig = Struct "scan_config"

-- FIXME: Get array types into this structure as marked
-- 
-- FIXME: Same deal below with ssid_ap/password_ap/channel_ap (should
-- be ssid/password/channel)
SQQ.fromStruct $ PA.StructDef "softap_config"
  [ PA.Field "ssid_ap" (PA.TyWord PA.Word8) L.NoLoc -- uint8[32]
  , PA.Field "password_ap" (PA.TyWord PA.Word8) L.NoLoc -- uint8[64]
  , PA.Field "ssid_len" (PA.TyWord PA.Word8) L.NoLoc
  , PA.Field "channel_ap" (PA.TyWord PA.Word8) L.NoLoc
  , PA.Field "authmode" (PA.TyWord PA.Word8) L.NoLoc -- AUTH_MODE?
  , PA.Field "ssid_hidden" (PA.TyWord PA.Word8) L.NoLoc
  , PA.Field "max_connection" (PA.TyWord PA.Word8) L.NoLoc
  , PA.Field "beacon_interval" (PA.TyWord PA.Word16) L.NoLoc
  ] L.NoLoc

-- | Equivalent to @struct softap_config@
type SoftAPConfig = Struct "softap_config"

-- FIXME: This is a more general type. Should it be elsewhere?
--
-- FIXME (maybe): 'ip_addr_t' is typedef'ed to 'struct ip_addr' and
-- it'd be nice to use the former.
SQQ.fromStruct $ PA.StructDef "ip_addr"
  [ PA.Field "addr" (PA.TyWord PA.Word32) L.NoLoc
  ] L.NoLoc

-- | Equivalent to @struct ip_addr@ or @ip_addr_t@
type IpAddr = Struct "ip_addr"

SQQ.fromStruct $ PA.StructDef "ip_info"
  [ PA.Field "ip"      (PA.TyStruct "ip_addr") L.NoLoc
  , PA.Field "netmask" (PA.TyStruct "ip_addr") L.NoLoc
  , PA.Field "gw"      (PA.TyStruct "ip_addr") L.NoLoc
  ] L.NoLoc

-- | Equivalent to @struct ip_info@
type IpInfo = Struct "ip_addr"

wifi :: Module
wifi = package "wifi" $ do
  defStruct (Proxy :: Proxy "station_config")
  defStruct (Proxy :: Proxy "scan_config")
  defStruct (Proxy :: Proxy "softap_config")
  defStruct (Proxy :: Proxy "ip_addr")
  defStruct (Proxy :: Proxy "ip_info")
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

wifi_station_ap_number_set :: Def('[Uint8] ':-> IBool)
wifi_station_ap_number_set = importProc "wifi_station_ap_number_set" "user_interface.h"

-- FIXME: Need array of StationConfig for argument
wifi_station_get_ap_info :: Def('[] ':-> Uint8)
wifi_station_get_ap_info = importProc "wifi_station_get_ap_info" "user_interface.h"

wifi_station_ap_change :: Def('[Uint8] ':-> IBool)
wifi_station_ap_change = importProc "wifi_station_ap_change" "user_interface.h"

wifi_station_get_current_ap_id :: Def('[] ':-> Uint8)
wifi_station_get_current_ap_id = importProc "wifi_station_get_current_ap_id" "user_interface.h"

wifi_station_get_auto_connect :: Def('[] ':-> Uint8)
wifi_station_get_auto_connect = importProc "wifi_station_get_auto_connect" "user_interface.h"

wifi_station_set_auto_connect :: Def('[Uint8] ':-> IBool)
wifi_station_set_auto_connect = importProc "wifi_station_set_auto_connect" "user_interface.h"

wifi_station_dhcpc_start :: Def('[] ':-> IBool)
wifi_station_dhcpc_start = importProc "wifi_station_dhcpc_start" "user_interface.h"

wifi_station_dhcpc_stop :: Def('[] ':-> IBool)
wifi_station_dhcpc_stop = importProc "wifi_station_dhcpc_stop" "user_interface.h"

-- | Equivalent to @dhcp_status@
type DhcpStatus = Sint32

-- | Equivalent to @DHCP_STOPPED@ of @dhcp_status@
status_dhcp_stopped :: DhcpStatus
status_dhcp_stopped = extern "DHCP_STOPPED" "user_interface.h"

-- | Equivalent to @DHCP_STARTED@ of @dhcp_status@
status_dhcp_started :: DhcpStatus
status_dhcp_started = extern "DHCP_STARTED" "user_interface.h"

wifi_station_dhcpc_status :: Def('[] ':-> DhcpStatus)
wifi_station_dhcpc_status = importProc "wifi_station_dhcpc_status" "user_interface.h"

wifi_station_dhcpc_set_maxtry :: Def('[Uint8] ':-> IBool)
wifi_station_dhcpc_set_maxtry = importProc "wifi_station_dhcpc_set_maxtry" "user_interface.h"

wifi_station_set_reconnect_policy :: Def('[IBool] ':-> IBool)
wifi_station_set_reconnect_policy = importProc "wifi_station_set_reconnect_policy" "user_interface.h"

wifi_station_get_rssi :: Def('[] ':-> Sint8)
wifi_station_get_rssi = importProc "wifi_station_get_rssi" "user_interface.h"

wifi_station_set_hostname :: Def('[IString] ':-> IBool)
wifi_station_set_hostname = importProc "wifi_station_set_hostname" "user_interface.h"

-- FIXME - Is this the right way to return a string?
wifi_station_get_hostname :: Def('[] ':-> CBytes s)
wifi_station_get_hostname = importProc "wifi_station_get_hostname" "user_interface.h"

wifi_softap_get_config :: Def('[Ref s SoftAPConfig] ':-> IBool)
wifi_softap_get_config = importProc "wifi_softap_get_config" "user_interface.h"

wifi_softap_get_config_default :: Def('[Ref s SoftAPConfig] ':-> IBool)
wifi_softap_get_config_default = importProc "wifi_softap_get_config_default" "user_interface.h"

wifi_softap_set_config :: Def('[Ref s SoftAPConfig] ':-> IBool)
wifi_softap_set_config = importProc "wifi_softap_set_config" "user_interface.h"

wifi_softap_set_config_current :: Def('[Ref s SoftAPConfig] ':-> IBool)
wifi_softap_set_config_current = importProc "wifi_softap_set_config_current" "user_interface.h"

wifi_softap_get_station_num :: Def('[] ':-> Uint8)
wifi_softap_get_station_num = importProc "wifi_softap_get_station_num" "user_interface.h"

-- Gaaaah.
-- struct station_info {
--        STAILQ_ENTRY(station_info)      next;
--        uint8 bssid[6];
--        struct ip_addr ip;
-- };
-- FIXME (auto-generated)
wifi_softap_get_station_info :: Def('[] ':-> ())
wifi_softap_get_station_info = importProc "wifi_softap_get_station_info" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_free_station_info :: Def('[] ':-> ())
wifi_softap_free_station_info = importProc "wifi_softap_free_station_info" "user_interface.h"

wifi_softap_dhcps_start :: Def('[] ':-> IBool)
wifi_softap_dhcps_start = importProc "wifi_softap_dhcps_start" "user_interface.h"

wifi_softap_dhcps_stop :: Def('[] ':-> IBool)
wifi_softap_dhcps_stop = importProc "wifi_softap_dhcps_stop" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_set_dhcps_lease :: Def('[] ':-> ())
wifi_softap_set_dhcps_lease = importProc "wifi_softap_set_dhcps_lease" "user_interface.h"

-- FIXME (auto-generated)
wifi_softap_get_dhcps_lease :: Def('[] ':-> ())
wifi_softap_get_dhcps_lease = importProc "wifi_softap_get_dhcps_lease" "user_interface.h"

wifi_softap_set_dhcps_lease_time :: Def('[Uint32] ':-> IBool)
wifi_softap_set_dhcps_lease_time = importProc "wifi_softap_set_dhcps_lease_time" "user_interface.h"

wifi_softap_get_dhcps_lease_time :: Def('[] ':-> Uint32)
wifi_softap_get_dhcps_lease_time = importProc "wifi_softap_get_dhcps_lease_time" "user_interface.h"

wifi_softap_reset_dhcps_lease_time :: Def('[] ':-> IBool)
wifi_softap_reset_dhcps_lease_time = importProc "wifi_softap_reset_dhcps_lease_time" "user_interface.h"

wifi_softap_dhcps_status :: Def('[] ':-> DhcpStatus)
wifi_softap_dhcps_status = importProc "wifi_softap_dhcps_status" "user_interface.h"

-- | Equivalent to @dhcps_offer_option@
type DhcpsOfferOption = Uint8

-- | Equivalent to @OFFER_START@ of @dhcps_offer_option@
offer_start :: DhcpsOfferOption
offer_start = extern "OFFER_START" "user_interface.h"

-- | Equivalent to @OFFER_ROUTER@ of @dhcps_offer_option@
offer_router :: DhcpsOfferOption
offer_router = extern "OFFER_ROUTER" "user_interface.h"

-- | Equivalent to @OFFER_END@ of @dhcps_offer_option@
offer_end :: DhcpsOfferOption
offer_end = extern "OFFER_END" "user_interface.h"

-- FIXME: This void* is a bit hairy
wifi_softap_set_dhcps_offer_option :: Def('[DhcpsOfferOption, Ptr s (Stored ())] ':-> IBool)
wifi_softap_set_dhcps_offer_option = importProc "wifi_softap_set_dhcps_offer_option" "user_interface.h"

-- | Equivalent to @phy_mode@
type PhyMode = Sint32

-- | Equivalent to @PHY_MODE_11B@ of @phy_mode@
phy_mode_11b :: PhyMode
phy_mode_11b = extern "PHY_MODE_11B" "user_interface.h"

-- | Equivalent to @PHY_MODE_11G@ of @phy_mode@
phy_mode_11g :: PhyMode
phy_mode_11g = extern "PHY_MODE_11G" "user_interface.h"

-- | Equivalent to @PHY_MODE_11N@ of @phy_mode@
phy_mode_11n :: PhyMode
phy_mode_11n = extern "PHY_MODE_11N" "user_interface.h"

wifi_set_phy_mode :: Def('[PhyMode] ':-> IBool)
wifi_set_phy_mode = importProc "wifi_set_phy_mode" "user_interface.h"

wifi_get_phy_mode :: Def('[] ':-> PhyMode)
wifi_get_phy_mode = importProc "wifi_get_phy_mode" "user_interface.h"

wifi_get_ip_info :: Def('[Uint8, Ref s IpInfo] ':-> ())
wifi_get_ip_info = importProc "wifi_get_ip_info" "ip_addr.h"

wifi_set_ip_info :: Def('[Uint8, Ref s IpInfo] ':-> IBool)
wifi_set_ip_info = importProc "wifi_set_ip_info" "ip_addr.h"

-- FIXME: Turn 2nd argument to be uint8[6]
wifi_set_macaddr :: Def('[Uint8, CBytes s] ':-> IBool)
wifi_set_macaddr = importProc "wifi_set_macaddr" "user_interface.h"

-- FIXME: Turn 2nd argument to be uint8[6]
wifi_get_macaddr :: Def('[Uint8, CBytes s] ':-> IBool)
wifi_get_macaddr = importProc "wifi_get_macaddr" "user_interface.h"

-- | Equivalent to @enum sleep_type@
type SleepType = Sint32

-- | Equivalent to @NONE_SLEEP_T@ of @enum sleep_type@
none_sleep_t :: SleepType
none_sleep_t = extern "NONE_SLEEP_T" "user_interface.h"

-- | Equivalent to @LIGHT_SLEEP_T@ of @enum sleep_type@
light_sleep_t :: SleepType
light_sleep_t = extern "LIGHT_SLEEP_T" "user_interface.h"

-- | Equivalent to @MODEM_SLEEP_T@ of @enum sleep_type@
modem_sleep_t :: SleepType
modem_sleep_t = extern "MODEM_SLEEP_T" "user_interface.h"

wifi_set_sleep_type :: Def('[SleepType] ':-> IBool)
wifi_set_sleep_type = importProc "wifi_set_sleep_type" "user_interface.h"

wifi_get_sleep_type :: Def('[] ':-> SleepType)
wifi_get_sleep_type = importProc "wifi_get_sleep_type" "user_interface.h"

wifi_status_led_install :: Def('[Uint8, Uint32, Uint8] ':-> ())
wifi_status_led_install = importProc "wifi_status_led_install" "user_interface.h"

wifi_status_led_uninstall :: Def('[] ':-> ())
wifi_status_led_uninstall = importProc "wifi_status_led_uninstall" "user_interface.h"

wifi_set_broadcast_if :: Def('[Uint8] ':-> IBool)
wifi_set_broadcast_if = importProc "wifi_set_broadcast_if" "user_interface.h"

wifi_get_broadcast_if :: Def('[] ':-> Uint8)
wifi_get_broadcast_if = importProc "wifi_get_broadcast_if" "user_interface.h"

{--
user_interface.h:
typedef void (* wifi_event_handler_cb_t)(System_Event_t *event);
typedef struct _esp_event {
    uint32 event;
    Event_Info_u event_info;
} System_Event_t;

-- How do I do a union in Ivory? Ugggh...
typedef union {
        Event_StaMode_Connected_t                       connected;
        Event_StaMode_Disconnected_t            disconnected;
        Event_StaMode_AuthMode_Change_t         auth_change;
        Event_StaMode_Got_IP_t                          got_ip;
        Event_SoftAPMode_StaConnected_t         sta_connected;
        Event_SoftAPMode_StaDisconnected_t      sta_disconnected;
        Event_SoftAPMode_ProbeReqRecved_t   ap_probereqrecved;
} Event_Info_u;

--}

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
