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

module RankNTypes where

applyToFive :: (forall a . a -> a) -> Int
applyToFive f = f 5

-- exercise 6.3-i
-- rank of Int -> forall a. a -> a
-- rank of (a -> b) -> (forall c. c -> a) -> b
-- rank of ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a

-- Continuation Monad

cont :: a -> (forall r . (a -> r) -> r)
cont a cb = cb a

runCont :: (forall r . (a -> r) -> r) -> a
runCont f = f id

-- Identity a isomorphism to a isomorphism to forall r. (a -> r) -> r
-- Identity a is a Monad, thus CPS forms a Monad
newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r }

-- Exercise 6.4-i
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap fab conta =
    Cont $ \f -> unCont conta (f . fab)

-- Exercise 6.4-ii
instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \f -> f a
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  contfab <*> conta = Cont $ \f ->
    unCont contfab $ \fa ->
    unCont conta $ \a -> f (fa a)

-- Exercise 6.4-iii
instance Monad Cont where
  return :: a -> Cont a
  return = pure
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  conta >>= fconta = unCont conta $ \a -> fconta a

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083356

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseString :: String
releaseString = withVersionNumber $ \v ->
  withTimestamp $ \t -> withOS $ \os -> os ++ "-" ++ show v ++ "-" ++ show t

-- monadic style
releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  v  <- Cont withVersionNumber
  t  <- Cont withTimestamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show v ++ "-" ++ show t
