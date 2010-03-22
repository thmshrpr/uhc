{-# INCLUDE "HsBase.h" #-}
{-# LINE 1 "System/CPUTime.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "System/CPUTime.hsc" #-}
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

module System.CPUTime 
        (
         getCPUTime,       -- :: IO Integer
         cpuTimePrecision  -- :: Integer
        ) where

import Prelude

import Data.Ratio


{-# LINE 28 "System/CPUTime.hsc" #-}


{-# LINE 32 "System/CPUTime.hsc" #-}


{-# LINE 34 "System/CPUTime.hsc" #-}
import Foreign
import Foreign.C


{-# LINE 38 "System/CPUTime.hsc" #-}

{-# LINE 39 "System/CPUTime.hsc" #-}

-- [###] extracted as local defintion
-- Converts a real to an integer by rounding it.
realToInteger :: Real a => a -> Integer 
realToInteger x = round (realToFrac x :: Double) -- [###] if I don't put double it leads to an ambiguous type)


{-# LINE 41 "System/CPUTime.hsc" #-}
-- -----------------------------------------------------------------------------
-- |Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.

getCPUTime :: IO Integer
getCPUTime = do


{-# LINE 50 "System/CPUTime.hsc" #-}
-- getrusage() is right royal pain to deal with when targetting multiple
-- versions of Solaris, since some versions supply it in libc (2.3 and 2.5),
-- while 2.4 has got it in libucb (I wouldn't be too surprised if it was back
-- again in libucb in 2.6..)
--
-- Avoid the problem by resorting to times() instead.
--

{-# LINE 58 "System/CPUTime.hsc" #-}
    allocaBytes (144) $ \ p_rusage -> do
{-# LINE 59 "System/CPUTime.hsc" #-}
    getrusage (0) p_rusage
{-# LINE 60 "System/CPUTime.hsc" #-}

    let ru_utime = ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_rusage
{-# LINE 62 "System/CPUTime.hsc" #-}
    let ru_stime = ((\hsc_ptr -> hsc_ptr `plusPtr` 16)) p_rusage
{-# LINE 63 "System/CPUTime.hsc" #-}
    u_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_utime :: IO CTime
{-# LINE 64 "System/CPUTime.hsc" #-}
    u_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_utime :: IO CTime
{-# LINE 65 "System/CPUTime.hsc" #-}
    s_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_stime :: IO CTime
{-# LINE 66 "System/CPUTime.hsc" #-}
    s_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_stime :: IO CTime
{-# LINE 67 "System/CPUTime.hsc" #-}
    return ((realToInteger u_sec * 1000000 + realToInteger u_usec + 
             realToInteger s_sec * 1000000 + realToInteger s_usec) 
                * 1000000)

type CRUsage = ()
foreign import ccall unsafe "HsBase.h getrusage" getrusage :: CInt -> Ptr CRUsage -> IO CInt

{-# LINE 93 "System/CPUTime.hsc" #-}


{-# LINE 127 "System/CPUTime.hsc" #-}

{-# LINE 128 "System/CPUTime.hsc" #-}

-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.


{-# LINE 134 "System/CPUTime.hsc" #-}
cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % fromIntegral (clockTicks))

{-# LINE 137 "System/CPUTime.hsc" #-}


{-# LINE 139 "System/CPUTime.hsc" #-}
clockTicks :: Int
clockTicks =

{-# LINE 144 "System/CPUTime.hsc" #-}
    unsafePerformIO (sysconf (2) >>= return . fromIntegral)
{-# LINE 145 "System/CPUTime.hsc" #-}
foreign import ccall unsafe sysconf :: CInt -> IO CLong

{-# LINE 147 "System/CPUTime.hsc" #-}

{-# LINE 148 "System/CPUTime.hsc" #-}
