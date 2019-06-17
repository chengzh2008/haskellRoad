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
{-# LANGUAGE RoleAnnotations #-}

module Roles where

import           Data.Coerce                    ( Coercible(..)
                                                , coerce
                                                )
import           Data.Foldable                  ( toList )
import qualified Data.Map                      as M
import           Data.Monoid                    ( Sum(..)
                                                , Product(..)
                                                )


newtype Reverse a = Reverse
  { getReverse :: a } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

-- sstrenghen a role
data BST v
  = Empty
  | Branch (BST v) v (BST v)

type role BST nominal
