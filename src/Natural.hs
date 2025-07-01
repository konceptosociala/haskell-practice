module Natural(
  Nat(..), beside, beside2, pow, natToInt,
  BinTree(..), treeDepth, treeReverse, treeSum, treeLeaves,
) where

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Ord)

instance Num Nat where
  abs a = a
  signum Zero = Zero
  signum _    = Succ Zero
  (+) a b = addNat (min a b) (max a b)
  (*) _ Zero      = Zero
  (*) a (Succ b)  = a + a * b
  negate _ = error "negate is undefined for Nat"
  fromInteger 0 = Zero
  fromInteger n = Succ ( fromInteger (n - 1) )

addNat :: Nat -> Nat -> Nat
addNat a Zero      = a
addNat a (Succ b)  = Succ (a + b)

beside :: Nat -> Nat -> Bool
beside Zero (Succ Zero) = True
beside (Succ Zero) Zero = True
beside (Succ a) (Succ b)
  | a == Succ b = True
  | b == Succ a = True
  | otherwise = False
beside _ _ = False

beside2 :: Nat -> Nat -> Bool
beside2 Zero (Succ (Succ Zero)) = True
beside2 (Succ (Succ Zero)) Zero = True
beside2 (Succ a) (Succ b)
  | a == Succ (Succ b) = True
  | b == Succ (Succ a) = True
  | otherwise = False
beside2 _ _ = False

pow :: Nat -> Nat -> Nat
pow Zero _ = Zero
pow _ Zero = Succ Zero
pow a (Succ b) = a * pow a b

natToInt :: Nat -> Int
natToInt Zero     = 0
natToInt (Succ a) = 1 + natToInt a

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)

instance Show a => Show (BinTree a) where
  show (Leaf a) = "Leaf " ++ show a
  show (Node a b) = "Node (" ++ show a ++ ") (" ++ show b ++ ")"

treeReverse :: BinTree a -> BinTree a
treeReverse (Leaf a) = Leaf a
treeReverse (Node a b) = Node (treeReverse b) (treeReverse a)

treeSum :: Num a => BinTree a -> a
treeSum (Leaf a) = a
treeSum (Node a b) = treeSum a + treeSum b

treeDepth :: BinTree a -> Nat
treeDepth (Leaf _) = Succ Zero
treeDepth (Node a b) = Succ (max (treeDepth a) (treeDepth b))

treeLeaves :: BinTree a -> [a]
treeLeaves (Leaf a) = [a]
treeLeaves (Node a b) = treeLeaves a ++ treeLeaves b