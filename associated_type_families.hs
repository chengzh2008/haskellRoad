{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RoleAnnotations #-}

module AssociatedTypeFamilies where

import           Data.Kind                      ( Type )
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.TypeLits

-- binary type constructor
data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type  Printf text = String
  format s _ = s <> symbolVal (Proxy @text)

instance (KnownSymbol text, HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (Show param, HasPrintf a) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""
