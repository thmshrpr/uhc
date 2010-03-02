module UHC.TestX ({-(<.>)-}) where -- [###] [BUG] cannot export operators?

import Test 

import UHC.Base
dummy = 1 :: Int

infixr 7  <.>
(<.>) :: Int -> Int -> Int
(<.>) = (+)
