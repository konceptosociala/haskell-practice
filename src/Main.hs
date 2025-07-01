module Main (main) where

import Room
import Natural

main :: IO ()
main = do
  let chair = Chair (Size 1.0 1.0 1.0) (Pos 0.0 0.0 0.0)
  let sofa = Sofa (Size 2.0 1.0 0.8) (Pos 2.0 0.0 0.0)
  let closet = Closet (Size 1.5 0.5 2.0) (Pos 1.0 1.0 0.0) Plastic

  print (
      Room [
        Instance chair, 
        Instance sofa, 
        Instance closet
      ]
    )

  let nat = 3 * (2 * 2 + 1) :: Nat
  let float = 3 * (2 * 2 + 1) :: Float
  let int = 3 * (2 * 2 + 1) :: Integer

  print "3 * (2 * 2 + 1):"
  print ("nat: "++show (natToInt nat))
  print ("float: "++show float)
  print ("int: "++show int)

  print ("beside 1 2: "++show (beside 1 2))
  print ("beside 2 1: "++show (beside 2 1))
  print ("beside 1 1: "++show (beside 1 1))

  print ("beside2 8 6: "++show (beside2 8 6))
  print ("beside2 8 9: "++show (beside2 8 9))
  print ("beside2 4 6: "++show (beside2 4 6))

  print ("3^4 = "++show (natToInt (pow 3 4)))
  print ("0^2 = "++show (natToInt (pow 0 2)))
  print ("2^0 = "++show (natToInt (pow 2 0)))

  let tree = Node 
        (Node 
          (Node 
            (Leaf 2) 
            (Leaf 4)
          ) 
          (Leaf 3)
        ) 
        (Leaf 1) 
        :: BinTree Integer

  print ("tree = "++show tree)
  print ("treeReverse tree = "++show (treeReverse tree))
  print ("treeSum tree = "++show (treeSum tree))
  print ("treeDepth tree = "++show (natToInt (treeDepth tree)))
  print ("treeLeaves tree = "++show (treeLeaves tree))

  