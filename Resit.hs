https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Resit where

import Data.Maybe (catMaybes)
import Data.List (transpose)
import System.Random
import Text.Printf (printf)

-- Importing the keymap module

import KeymapList
-- import KeymapTable

-- Type declarations
type Barcode   = String
type Name      = String
type Price     = Int -- in pence
type Item      = (Name, Price)
type Catalogue = Keymap Barcode Item

-- A little test catalog
testDB :: Catalogue
testDB = fromList
  [ ("0265090316581", ("Talisker Single Malt Whisky", 2900))
  , ("0903900739533", ("Bagpipes of Glory", 1000))
  , ("9780201342758", ("Hutton - Programming in Haskell", 3500))
  , ("0042400212509", ("Frying pan", 750))
  ]

-- Show pence as pounds with two decimal points
showPrice :: Int -> String
showPrice total = printf "£%d.%02d" pounds pence
  where (pounds, pence) = total `divMod` 100

-- Show an item as a list of strings, to be padded later
showItem :: (Barcode, Item) -> [String]
showItem (k, (n, p)) = [k , n , showPrice p]

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '

showCatalogue :: Catalogue -> String
showCatalogue c = format (map showLine ls)
  where
    ls = ["Key", "Name", "Price"] : map showItem (toList c)

    -- Compute the limits of each column.
    limits = map ((+ 3) . maximum . map length) (transpose ls)

    -- Show a single line, padding each entry appropriately (depending on its column).
    showLine :: [String] -> String
    showLine = concat . zipWith pad limits

    -- Add vertical line to the final output.
    format :: [String] -> String
    format (x:xs) = unlines (x:vline:xs)
      where vline = replicate (sum limits) '-'

{-
*Resit> putStrLn (showCatalogue testDB)
Key             Name                              Price
-----------------------------------------------------------
0265090316581   Talisker Single Malt Whisky       £29.00
0903900739533   Bagpipes of Glory                 £10.00
9780201342758   Hutton - Programming in Haskell   £35.00
0042400212509   Frying pan                        £7.50
-}

-- * Exercise 1: Displaying customer orders.

type Quantity = Int
type Order = [(Barcode, Quantity)]

testOrder :: Order
testOrder =
  [ ("0265090316581", 2)
  , ("9780201342758", 1)
  ]

showItem' :: ((Name, Price, Quantity), Int, Int) -> [String]
showItem' = undefined

showOrder :: Catalogue -> Order -> String
showOrder = undefined

{-
*Resit> putStrLn (showOrder testDB testOrder)
Name                              Qty   Rate     Amount   Running Sum
------------------------------------------------------------------------
Talisker Single Malt Whisky       2     £29.00   £58.00   £58.00
Hutton - Programming in Haskell   1     £35.00   £35.00   £93.00
------------------------------------------------------------------------
VAT (20%)                                                 + £18.60
                                                          = £111.60
-}

-- * Exercise 2: Retrieving list of items using the Keymap API.

getItems :: [Barcode] -> Catalogue -> [Item]
getItems = undefined


-- * Exercise 3: Benchmarking the list implementation.

{-
*Resit> :set +s
*Resit> db <- readDB
Done
(??? secs)
*Resit> ks <- samples 1000 db
(??? secs)
*Resit> force (getItems ks db)
()
(??? secs)
-}

-- * Exercise 8: Benchmarking the hash-table implementation.

{-
*Resit> :r
[2 of 3] Compiling KeymapTable      ( KeymapTable.hs, interpreted )
[3 of 3] Compiling Resit            ( Resit.hs, interpreted )
Ok, three modules loaded.
*Resit> db <- readDB
Done
(??? secs)
*Resit> ks <- loadKeys
(??? secs)
*Resit> force (getItems ks db)
()
(??? secs)
*Resit>
-}


-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- lines <$> readFile "database.csv"
            let db = fromList (map readLine dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode, Item)
readLine s = (i, (n, read p))
  where
    (i, s') = splitUpon ',' s
    (n, p)  = splitUpon ',' s'

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

samples :: Int -> Catalogue -> IO [Barcode]
samples n db =
  do g <- newStdGen
     let allKeys = [ key | (key,item) <- toList db ]
     let indices = randomRs (0, length allKeys - 1) g
     let keys = take n [ allKeys !! i | i <- indices ]
     saveKeys keys
     return (force keys `seq` keys)

saveKeys :: [Barcode] -> IO ()
saveKeys = writeFile "keys.cache" . show

loadKeys :: IO [Barcode]
loadKeys = do
  keys <- read <$> readFile "keys.cache"
  return (force keys `seq` keys)

force :: [a] -> ()
force = foldr seq ()
