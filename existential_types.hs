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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}

module ExistentialTypes where
import           Data.Kind
import           Data.Typeable
import           Data.Maybe
import           Data.Foldable
import           Data.IORef
import           System.IO.Unsafe               ( unsafePerformIO )

data Any where
  Any :: a -> Any

data HasShow where
  HasShow :: Show a => a -> HasShow

instance Show HasShow where
  show = elimHasShow show

elimHasShow :: (forall a . Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a . Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

-- TODO: fromDynamic $ Dynamic 6 returns Nothing, why?
-- seems run 'default (Int)' in repl solves the issue
fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
  :: forall a b r
   . (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

-- TODO: seems not working for the following example in ghci:
-- fromDynamic @String (pyPlus (Dynamic 4) (Dynamic " def"))
pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (error "bad types for pyPlus") $ asum
  [ liftD2 @String @String a b (++)
  , liftD2 @Int @Int a b (+)
  , liftD2 @String @Int a b $ \strA intB -> strA ++ show intB
  , liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
  ]

-- Generalized Consraint Kinded Existentials

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a . c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow' = Has Show
type Danymic' = Has Typeable

-- constraint synonym
class (Monoid a, Eq a, Show a) => MonoidEqShow a
instance (Monoid a, Eq a, Show a) => MonoidEqShow a

instance Show (Has MonoidEqShow) where
  show = elimHas show


-- implement ST monad
newtype ST s a = ST
  {
    unsafeRunST :: a
  }

-- make it explicitly strict using 'seq a'
instance Functor (ST s) where
  fmap f (ST a) = seq a $ ST $ f a

instance Applicative (ST s) where
  pure = ST
  (ST f) <*> (ST a) = seq f $ seq a $ ST $ f a

instance Monad (ST s) where
  (ST a) >>= f = seq a $ f a

newtype STRef s a = STRef
  { unSTRef :: IORef a
  }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef stref = pure . unsafePerformIO . writeIORef (unSTRef stref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST :: (forall s . ST s a) -> a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref
