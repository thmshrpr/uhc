{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Prelude: a standard module imported by default into all Haskell
-- modules.  For more documentation, see the Haskell 98 Report
-- <http://www.haskell.org/onlinereport/>.
--
-----------------------------------------------------------------------------

module Prelude
  ( module UHC.Base
  , module UHC.Eq
  , module UHC.Ord
  , module UHC.Enum
  , module UHC.Bounded
  , module UHC.Ix
  , module UHC.Show
  , module UHC.Read
  , module UHC.Run
--  , module UHC.Handle
--  , module System.IO
  , module UHC.IOBase
--  , unsafePerformIO
  , FilePath -- [###] temporarely, until we can import System.IO
  )
  where

import UHC.Base hiding
  ( absReal, signumReal
  -- , primEqInt
  -- , State, IOWorld, RealWorld
  , ByteArray
  , exitWithIntCode
  )
import UHC.Eq
import UHC.Ord
import UHC.Enum
import UHC.Bounded
import UHC.Ix
import UHC.Show
import UHC.Read
-- [###] Only this function are actualy exported by GHC prelude. Handle is not exported in the GHC Predude; is uhc design to export it?
import UHC.IOBase ( Handle, IOError, ioError, userError, catch, unsafePerformIO )
-- import UHC.Handle -- hiding ( hPutStrLn )
import UHC.Run

type FilePath = String -- [###] temporarely, until we can import System.IO
-- [###] There is a ?bug? that cause the cycle dependency: if we import (direct or indirect) in the Prelude a module which is not inside UHC directory we get "Mutually recursive modules". Is Prelude automatically imported for modules outside UHC? Is there a flag to disable this?
--  import UHC.TestX

-- [###] The IO function exported by  GHC prelude are from System.IO
{-
import System.IO
  ( -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn
  )
-}  


