{-# LANGUAGE MagicHash #-}

-- Try "ghc -ddump-ds GCDPrim.hs"

module GCDPrim where

import GHC.Exts

mygcd :: Int# -> Int# -> Int#

mygcd a b | a ==# b   = a
          | a <# b    = mygcd a (b -# a)
          | otherwise = mygcd (a -# b) b
                        
main :: Int# -> Int#
main x = mygcd x x
