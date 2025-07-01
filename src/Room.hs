{-# LANGUAGE GADTs #-}
module Room (
  Room(..), 
  Pos(..), Size(..), Material(..), 
  FurnitureInstance(..), Furniture(..),
  Chair(..), Sofa(..), Table(..), Closet(..)
) where

newtype Room = Room [FurnitureInstance]

instance Show Room where
  show (Room furniture) = "Room [\n" ++ unlines (map show furniture) ++ "]"

data Pos = Pos Float Float Float 
  deriving Show

data Size = Size Float Float Float 
  deriving Show

data Material = 
  Wood 
  | Metal 
  | Plastic 
  | Fabric 
  | Composite [Material] 
  deriving Show

data FurnitureInstance where
  Instance :: Furniture a => a -> FurnitureInstance

class Furniture a where
  size      :: a -> Size
  material  :: a -> Material
  position  :: a -> Pos

instance Show FurnitureInstance where
  show (Instance a) = "Furniture("++show (size a)++", "++show (material a)++", "++show (position a)++")"

data Chair = Chair Size Pos
instance Furniture Chair where
  size (Chair s _)  = s
  position (Chair _ p) = p
  material _ = Wood

data Sofa = Sofa Size Pos
instance Furniture Sofa where
  size (Sofa s _) = s
  position (Sofa _ p) = p
  material _ = Composite [Wood, Fabric]

data Table = Table Size Pos
instance Furniture Table where
  size (Table s _)  = s
  position (Table _ p) = p
  material _ = Wood

data Closet = Closet Size Pos Material
instance Furniture Closet where
  size (Closet s _ _) = s
  position (Closet _ p _) = p
  material (Closet _ _ m) = m