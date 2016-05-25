{-|
Module      : SpiFlash
Description : Ivory wrappers for SPI Flash API in Espressif SDK
Copyright   : (c) Chris Hodapp, 2016
License     : 
Maintainer  : hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

Unless otherwise specified, every Ivory procedure given here follows
its identically-named C function in the ESP8266 Non-OS SDK API Guide,
section 3.4 ("SPI Flash Related APIs").

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.ESP.SpiFlash where

import Ivory.Language.IvoryUtil

import Ivory.Language
import Ivory.Language.Type

import qualified Ivory.Language.Syntax.Concrete.ParseAST as PA
import qualified Ivory.Language.Syntax.Concrete.Location as L
import qualified Ivory.Language.Syntax.Concrete.QQ.StructQQ as SQQ

SQQ.fromStruct $ PA.StructDef "spiFlashChipT"
  [ PA.Field "deviceId"    (PA.TyWord PA.Word32) L.NoLoc
  , PA.Field "chip_size"   (PA.TyWord PA.Word32) L.NoLoc
  , PA.Field "block_size"  (PA.TyWord PA.Word32) L.NoLoc
  , PA.Field "sector_size" (PA.TyWord PA.Word32) L.NoLoc
  , PA.Field "page_size"   (PA.TyWord PA.Word32) L.NoLoc
  , PA.Field "status_mask" (PA.TyWord PA.Word32) L.NoLoc
  ] L.NoLoc

-- FIXME: This struct should just have type name "SpiFlashChip", no
-- "struct" in front of it.
type SpiFlashChip = Struct "spiFlashChipT"

spi_flash :: Module
spi_flash = package "spi_flash" $ do
  defStruct (Proxy :: Proxy "spiFlashChipT")
  incl spi_flash_get_id
  incl spi_flash_erase_sector
  incl spi_flash_write
  incl spi_flash_read
  incl system_param_save_with_protect
  incl system_param_load
  incl spi_flash_set_read_func

spi_flash_get_id :: Def('[] ':-> Uint32)
spi_flash_get_id = importProc "spi_flash_get_id" "spi_flash.h"

type SpiFlashOpResult = Sint32

-- FIXME: Is this the best way to render an enum?

-- | Enum value for @SPI_FLASH_RESULT_OK@
spiFlashResultOk :: SpiFlashOpResult
spiFlashResultOk = extern "SPI_FLASH_RESULT_OK" "spi_flash.h"

-- | Enum value for @SPI_FLASH_RESULT_ERR@
spiFlashResultErr :: SpiFlashOpResult
spiFlashResultErr = extern "SPI_FLASH_RESULT_ERR" "spi_flash.h"

-- | Enum value for @SPI_FLASH_RESULT_TIMEOUT@
spiFlashResultTimeout :: SpiFlashOpResult
spiFlashResultTimeout = extern "SPI_FLASH_RESULT_TIMEOUT" "spi_flash.h"

spi_flash_erase_sector :: Def('[Uint16] ':-> SpiFlashOpResult)
spi_flash_erase_sector = importProc "spi_flash_erase_sector" "spi_flash.h"

-- FIXME: Is this how the uint32* is to be used?
spi_flash_write :: Def('[Uint32, CBytes s, Uint32] ':-> SpiFlashOpResult)
spi_flash_write = importProc "spi_flash_write" "spi_flash.h"

-- FIXME: Same as spi_flash_write, is this how to use uint32*?
spi_flash_read :: Def('[Uint32, CBytes s, Uint32] ':-> SpiFlashOpResult)
spi_flash_read = importProc "spi_flash_read" "spi_flash.h"

-- FIXME: Correct way to render a void*?
system_param_save_with_protect :: Def('[Uint16, CBytes s, Uint16] ':-> IBool)
system_param_save_with_protect = importProc "system_param_save_with_protect" "user_interface.h"

system_param_load :: Def('[Uint16, Uint16, CBytes s, Uint16] ':-> IBool)
system_param_load = importProc "system_param_load" "user_interface.h"

-- | Equivalent to 'user_spi_flash_read'
type UserSpiFlashRead s t = '[Ref s SpiFlashChip, Uint32, CBytes t, Uint32] ':-> SpiFlashOpResult

spi_flash_set_read_func :: Def('[Uint16, Uint16, ProcPtr (UserSpiFlashRead s t), Uint16] ':-> IBool)
spi_flash_set_read_func = importProc "spi_flash_set_read_func" "spi_flash.h"

