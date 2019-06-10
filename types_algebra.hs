{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types_Algebra where

import           Data.Word
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           GHC.TypeLits
import           Data.Typeable
import           Data.Kind                      ( Constraint
                                                , Type
                                                )


-- Isomorphism
data Void
data Unit = Unit

data Booll = Falsee | Truee
data Spin = Up | Down

-- Booll and Spin are isomorphism
boolToSpin1 :: Booll -> Spin
boolToSpin1 Falsee = Up
boolToSpin1 Truee  = Down

spinToBool1 :: Spin -> Booll
spinToBool1 Up   = Falsee
spinToBool1 Down = Truee

-- Sum, Product and Exponential Types
{-
|Either a b| = |a| + |b|
-}

data Deal a b = This a | That b | TheOther Bool
{-
|Deal a b| = |a| + |b| + |Bool|
-}

data Maybee a = Justt a | Nothingg
{-
|Maybe a| = 1 + |a|
-}

data MixedFraction a = Fraction
  { mixedBit :: Word8
  , numerator :: a
  , denominator :: a
  }
{-
|MixedFraction a| = |Word8| x |a| x |b|
-}

-- 2^2 inhabitants off the type Bool -> Bool
-- id, not, const True and const False
-- data (->) a b = a -> b
{-
|(->) a b| = |b|^|a|
-}

-- Exercise 1.2-i
-- cardinality of the blowing data
-- data Either Bool (Bool, Maybe Bool) -> Bool
{-
2^(2 + (2 * 3)) = 2^8
-}

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not  'False = 'True

type family Foo (x :: Bool) (y :: Bool) :: Bool
type family Bar x y :: Bool -> Bool -> Bool

-- Type Scoping
broken :: (a -> b) -> a -> b
broken f a = apply
  where
  --apply :: b -- here b is different from the b above
        apply = f a
  --  apply :: c
  --  apply = f a
  --
working :: forall a b . (a -> b) -> a -> b
working f a = apply
 where
  apply :: b -- here b is different from the b above
  apply = f a

typeName :: forall a . Typeable a => String
typeName = show . typeRep $ Proxy @a

-- type equality
five :: Int
five = 5

five_ :: (a ~ Int) => a
five_ = 5

-- GADTs
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt  i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y  ) = evalExpr x + evalExpr y
evalExpr (Not x    ) = not $ evalExpr x
evalExpr (If b x y ) = if evalExpr b then evalExpr x else evalExpr y

-- TODO: why ~ can not be used here
data Expr_ a
  = LitInt_ Int
  | LitBool_ Bool
  | Add_ (Expr_ Int) (Expr_ Int)
  | Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

-- Heterogeneous Lists
data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

secondBool :: HList '[_1, Bool, _2] -> Bool
secondBool (_ :# b :# _ :# HNil) = b

{-
instance Eq (HList '[]) where
  HNil == HNil = True
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

instance Ord (HList '[]) where
  HNil <= HNil = True
instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  (a :# as) <= (b :# bs) = a <= b && as <= bs

instance Show (HList '[]) where
  show HNil = "Empty"
instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (a :# as) = show a <> ":" <> show as
-}

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil <= HNil = True
  (a :# as) <= (b :# bs) = a <= b && as <= bs

instance All Show ts => Show (HList ts) where
  show HNil = "Empty"
  show (a :# as) = show a <> ":" <> show as
