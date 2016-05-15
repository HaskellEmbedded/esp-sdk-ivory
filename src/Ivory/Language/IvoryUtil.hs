{-# LANGUAGE DataKinds, TypeOperators #-}

module Ivory.Language.IvoryUtil where

import Ivory.Language
import Ivory.Language.Array

-- | Convenience type for how we represent struct data in Ivory:
type ByteArray n = Array n (Stored Uint8)

-- | Convenience type for a byte array of size 'n'
type ByteArea n = MemArea (Array n (Stored Uint8))

-- | Convenience type for a C byte array (size unspecified)
type CBytes s = Ref s (CArray (Stored Uint8))
