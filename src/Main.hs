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
  print ("nat: "++show nat)
  print ("float: "++show float)
  print ("int: "++show int)

  print (
      "beside (Succ Zero) (Succ (Succ Zero)): " ++
      show (beside (Succ Zero) (Succ (Succ Zero)))
    )

  print (
      "beside (Succ (Succ Zero)) (Succ Zero): " ++
      show (beside (Succ (Succ Zero)) (Succ Zero))
    )

  print (
      "beside (Succ Zero) (Succ Zero): " ++
      show (beside (Succ Zero) (Succ Zero))
    )

  print (
      "beside2 (Succ Zero) (Succ (Succ Zero)): " ++
      show (beside2 (Succ Zero) (Succ (Succ Zero)))
    )

  print (
      "beside2 (Succ (Succ Zero)) (Succ Zero): " ++
      show (beside2 (Succ (Succ Zero)) (Succ Zero))
    )

  print (
      "beside2 (Succ Zero) (Succ Zero): " ++
      show (beside2 (Succ Zero) (Succ (Succ (Succ Zero))))
    )

  