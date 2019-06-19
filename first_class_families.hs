{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}



module FirstClassFamilies where
import           Prelude                 hiding ( fst )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )

data Fst a b = Fst (a, b)

-- typeclass with a functional dependency
class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

-- Exercise 10.1 -i
-- Defufnctionalize listToMaybe :: [a] -> Maybe
data List2Maybe a = List2Maybe [a]

instance Eval (List2Maybe a) (Maybe a) where
  eval (List2Maybe []) = Nothing
  eval (List2Maybe (a:_)) = Just a

-- defunctionalize higher order function
data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft=> Eval (MapList dfb a) [dft] where
  eval (MapList f as) = map (eval . f) as


-- Type-Level Defunctionalization
type Exp a = a -> Type

type family Eval2 (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval2 (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval2 (FromMaybe _1 ('Just a)) = a
type instance Eval2 (FromMaybe a 'Nothing) = a

-- exercise 10.2-i
-- Deffunctionalize listToMaybe at the type-level
data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval2 (ListToMaybe '[]) = 'Nothing
type instance Eval2 (ListToMaybe (a:_)) = 'Just a

data MapList2 :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval2 (MapList2 f '[]) = '[]
type instance Eval2 (MapList2 f (a:as)) = Eval2 (f a) : Eval2 (MapList2 f as)


-- Exercise 10.2-ii
-- Defunctionalize foldr
data FoldR :: (a -> b -> b) -> b -> [a] -> Exp b

type instance Eval2 (FoldR fab b '[]) = b
type instance Eval2 (FoldR fab b (a:as)) = Eval2 (FoldR fab (fab b a) as)


-- first-class families for a monad at the type-level
data Pure :: a -> Exp a

type instance Eval2 (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b

type instance Eval2 (k =<< e) = Eval2 (k (Eval2 e))
infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type instance Eval2 ((f <=< g) x) =  Eval2 (f (Eval2 (g x)))
infixr 1 <=<

-- <=< at the type-level acts like regular function composition (.),
-- =<< behaves like funciton application ($)

data TyEq :: a -> b -> Exp Bool
type instance Eval2 (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval2 (Collapse '[]) = (() :: Constraint)
type instance Eval2 (Collapse (a:as)) = (a, Eval2 (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) =
  Collapse =<< MapList2 (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval2 (Pure1 f x) = f x

-- Ad-Hoc Polymorphism
data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval2 (Map f '[]) = '[]
type instance Eval2 (Map f (a:as)) = Eval2 (f a) : Eval2 (Map f as)

type instance Eval2 (Map f 'Nothing) = 'Nothing
type instance Eval2 (Map f ('Just a)) = 'Just (Eval2 (f a))

type instance Eval2 (Map f ('Left x)) = 'Left x
type instance Eval2 (Map f ('Right y)) = 'Right (Eval2 (f y))

-- Exercise 10.4-i
-- promoted functor instance for tuple
type instance Eval2 (Map f '(a, x)) = '(Eval2 (f a), x)

{-
type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  a ++ '[] = a
  '[] ++ b = b
  (a:as) ++ bs = a : (as ++ bs)
-}

-- promoting mappend
-- TODO: Not in scope: type constructor or class ‘++’
data Mappend :: a -> a -> Exp a
type instance Eval2 (Mappend '() '()) = '()
type instance Eval2 (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance Eval2 (Mappend (a :: [k]) (b :: [k])) = (a ++ b)
