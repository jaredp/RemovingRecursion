{-# LANGUAGE MagicHash #-}

-- Try "ghc -ddump-ds AddPrim.hs"

module AddPrim where

import GHC.Exts

add :: Int# -> Int# -> Int#
add x y = x +# y

--sub :: Int# -> Int# -> Int#
--sub x y = x -# y
