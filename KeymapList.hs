https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-- Indexed data represented as a list

module KeymapList ( Keymap,
                    size,
                    get, set, del,
                    select,
                    toList, fromList,
                  )
where

import Test.QuickCheck
import Data.List (nub)
import Control.Monad (liftM2)

newtype Keymap k a = K [(k,a)] deriving Show

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
  arbitrary = K <$> liftM2 zip (nub <$> listOf arbitrary) arbitrary

size :: Eq k => Keymap k a -> Int
size (K xs) = length xs

get :: Eq k => k -> Keymap k a -> Maybe a
get key (K xs) = lookup key xs

set :: Eq k => k -> a -> Keymap k a -> Keymap k a
set key value (K xs) = K (ins  xs)
    where
      ins [] = [(key,value)]
      ins ((k,v):xs) | k == key  = (k,value) : xs
                     | otherwise = (k,v) : ins xs

del :: Eq k => k -> Keymap k a -> Keymap k a
del key (K xs) = K (filter ((/=key).fst) xs)

select :: Eq k => (a -> Bool) -> Keymap k a -> Keymap k a
select f (K xs) = K (filter (f.snd) xs)

toList :: Eq k => Keymap k a -> [(k,a)]
toList (K xs) = xs

fromList :: Eq k => [(k,a)] -> Keymap k a
fromList xs = K xs
