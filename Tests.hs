module AVL.Tests where

import Test.QuickCheck
import AVL

-- balance is always -1, 0 or 1
prop_balance :: (Ord a, Show a) => [a] -> Bool
prop_balance xs =
  let
    b = getBalance (avlTree xs)
  in
    b == -1 || b == 0 || b == 1

-- do some testing
main = quickCheck (prop_balance :: [Int] -> Bool)
