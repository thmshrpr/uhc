module UHC.TestX  where -- [###] [BUG] cannot export operators?

--import Test 
import UHC.Base
import UHC.IO
import UHC.IOBase
import UHC.Handle
dummy = 1 :: Int

infixr 7  <.>

(<.>) :: Int -> Int -> Int
(<.>) = (+)

--f = 1 <.> 1
typeBug:: IO String
typeBug = 
  let
        loop x
           | x == [] = return ""
           | otherwise = 
              do
               if x == ""
                  then return ""
                  else loop x
        loop' raw r = loop raw r -- [###] to avoid type error "Cannot derive coercion for type application"
  in do
  t <- loop []
  return t

hGetLineBufferedLoop :: Handle__ -> IORef Buffer -> Buffer -> [String]
                     -> IO String
hGetLineBufferedLoop handle_ ref
        buf@Buffer{ bufRPtr=r0, bufWPtr=w, bufBuf=raw0 } xss =
  let
        -- find the end-of-line character, if there is one
        loop:: RawBuffer -> Int -> IO (Bool, Int)
        loop raw r 
           | r == w    = return (False, w)
           | otherwise = 
              do
               (c,r') <- readCharFromBuffer raw r 
               if c == '\n'
                  then return (True, r) -- NB. not r': don't include the '\n'
                  else loop raw r'
  in do
  return []

loop:: RawBuffer -> Int -> Int -> IO (Bool, Int)
loop raw r w
   | r == w    = return (False, w)
   | otherwise = 
      do
         (c,r') <- readCharFromBuffer raw r 
         if c == '\n'
            then return (True, r) -- NB. not r': don't include the '\n'
            else loop raw r' w

