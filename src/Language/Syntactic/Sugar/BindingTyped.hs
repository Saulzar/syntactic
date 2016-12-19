{-# LANGUAGE UndecidableInstances #-}

-- | 'Syntactic' instance for functions for domains based on 'Typed' and
-- 'BindingT'

module Language.Syntactic.Sugar.BindingTyped where



import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Functional

import Data.Proxy

type TypedBinding = Typed BindingT

instance
    ( sym ~ Typed s
    , Syntactic a, Domain a ~ sym
    , Syntactic b, Domain b ~ sym
    , BindingT :<: s
    , Typeable (Internal a)
    , Typeable (Internal b)
    ) =>
      Syntactic (a -> b)
  where
    type Domain (a -> b)   = Domain a
    type Internal (a -> b) = Internal a -> Internal b
    desugar f = lam (Proxy @TypedBinding) (desugar . f . sugar)
    sugar     = error "sugar not implemented for (a -> b)"
