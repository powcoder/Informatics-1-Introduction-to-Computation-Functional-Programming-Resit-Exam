https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-- ** Faster Keymap implementation using hash tables.
{-# LANGUAGE ScopedTypeVariables #-}
module KeymapTable
  ( gather
  , hash
  , Keymap
  , fromList
  , get
  , toList
  , size
  ) where

import Data.Char (ord)
import GHC.Arr

import qualified KeymapList as L


-- * Exercise 4: gather

gather :: Ix i => (i,i) -> [(i,a)] -> [(i,[a])]
gather = undefined

-- Hashes and hash tables

type Hash = Int

data Keymap k v = HashTable
  (Array Hash (L.Keymap k v))  -- an array mapping hashes to keymap lists
  Int                          -- capacity
  deriving Show

class Hashable a where
  hash :: a -> Hash

hashMod :: Hashable a => a -> Int -> Hash
hashMod x cap = hash x `mod` cap

instance Hashable Char where
  hash = ord

-- * Exercise 5: hashing a list

instance Hashable a => Hashable [a] where
  hash s = undefined

-- * Exercise 6: fromList

fromList :: forall k v. (Eq k, Hashable k) => [(k, v)] -> Keymap k v
fromList kvs = HashTable arr2 capacity
  where
    capacity :: Int
    capacity = length kvs

    -- * Exercise 6a: calculating the list-valued array
    arr :: Array Hash [(k, v)]
    arr = undefined

    -- * Exercise 6b: calculating the keymap-valued array
    arr2 :: Array Hash (L.Keymap k v)
    arr2 = undefined

-- * Exercise 7: get/toList/size

get :: (Eq k, Hashable k) => k -> Keymap k v -> Maybe v
get = undefined

toList :: (Eq k, Hashable k) => Keymap k v -> [(k, v)]
toList = undefined

size :: Eq k => Keymap k a -> Int
size = undefined
