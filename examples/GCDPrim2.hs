{-# LANGUAGE MagicHash #-}

-- Try "ghc -ddump-ds GCDPrim.hs"

module GCDPrim2 where

import GHC.Exts

mygcd :: Int# -> Int# -> Int#
mygcd a b | a ==# b   = a
          | a <# b    = mygcd a (b -# a)
          | otherwise = mygcd (a -# b) b
                      
add2 :: Int# -> Int# -> Int#
add2 a b = a +# b
                        
top :: Int# -> Int#
top _ = 33# +# add2 (mygcd 255# 370#) (mygcd 16# 36#)

