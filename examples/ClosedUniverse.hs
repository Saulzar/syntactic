{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}


{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- | This module demonstrates the use of 'WS' terms. In particular, note that 'share' has no
-- constraints on the type @a@ in contrast to the corresponding function in NanoFeldspar.
--
-- 'WS' terms can be evaluated directly using 'evalClosedWS' and they can be examined by first
-- converting them using the function 'fromWS'.

module ClosedUniverse where



import Language.Syntactic
import Language.Syntactic.Functional


data Prim = I | F | B

data Number (sig :: Sig Prim) where
    Lit :: Interp p -> Number (Full p)
    Add :: Number (p :-> p :-> Full p)
    Sub :: Number (p :-> p :-> Full p)
    Mul :: Number (p :-> p :-> Full p)



-- instance Num (Interp p) => Num (Number (Full p)) where
--   fromInteger = Lit . fromInteger
--   (+) = Add


type family Interp (p :: Prim) = (t :: *) | t -> p where
  Interp I  = Int
  Interp F  = Float
  Interp B  = Bool


type family InterpSig (sig :: Sig Prim) = (r :: Sig *) where
  InterpSig (p :-> a) = Interp p :-> InterpSig a
  InterpSig (Full p) = Full (Interp p)

data InterpDom sym p where
  InterpSym :: sym p -> InterpDom sym (InterpSig p)


type Dom = Number :+: Let
type Exp p = ASTF Dom p


instance (Num (Interp p), Number :<: dom) => Num (ASTF dom p) where
  fromInteger = inj . Lit . fromInteger
  (+) = sugarSym Add

type InterpExp sig = AST (InterpDom Dom) sig
