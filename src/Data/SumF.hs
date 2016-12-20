
module Data.SumF where

import GHC.Generics (Generic1)

import Data.Kind (Type, Constraint)
import Data.Proxy

import Data.Functor.Classes

import Type.Family.List
import Data.Type.Index
import Data.Type.Remove

import Type.Class.Witness
import Prelude

import Control.Arrow
import Data.Either

import Data.Inject


data SumF :: [k -> Type] -> k -> Type where
  L :: f a -> SumF (f : fs) a
  R :: SumF fs a -> SumF (f : fs) a


instance (Elem fs f) => (:<:) f (SumF fs) where
  inj = inj' elemIndex

inj' :: Index fs f -> f a -> SumF fs a
inj' = \case
  IZ   -> L
  IS x -> R . inj' x


instance (Elem fs f) => Project f (SumF fs) where
  prj = prj' elemIndex


prj' :: Index fs f -> SumF fs a -> Maybe (f a)
prj' IZ (L x)     = Just x
prj' IZ (R _)     = Nothing
prj' (IS n) (R x) = prj' n x
prj' (IS _) (L _) = Nothing


-- instance Project f (SumF (f:fs)) where
--   prj (L x) = Just x
--   prj _     = Nothing
--
-- instance Project g (SumF fs) => Project g (SumF (f:fs)) where
--   prj (R ys) = prj ys

elim :: forall c fs a r. (Every c fs) => (forall f. Wit (c f) -> f a -> r) -> SumF fs a -> r
elim f (L x)  = f Wit x
elim f (R fs) = elim f fs


showsPrec1' :: (Show a) => Wit (Show1 f) -> Int -> f a -> ShowS
showsPrec1' Wit d x = showParen (d > 10) $ showString "SumF " . showsPrec1 11 x

instance (Every Show1 fs, Show a) => Show (SumF fs a) where
  showsPrec n = elim (flip showsPrec1' n)

instance (Every Functor fs) => Functor (SumF fs) where
  fmap f (L x)  = L (fmap f x)
  fmap f (R fs) = R (fmap f fs)


replace ::  (Without fs f gs, Elem gs g) => (f a -> g a) -> SumF fs a -> SumF gs a
replace f = either (inj . f) id . remove' without

remove ::  Without fs f gs => proxy f -> SumF fs a -> Either (f a) (SumF gs a)
remove _ = remove' without

remove' ::  Remove fs f gs -> SumF fs a -> Either (f a) (SumF gs a)
remove'  RZ (L x) = Left x
remove'  RZ (R xs) = Right xs
remove'  (RS n) (L x) = Right (L x)
remove'  (RS n) (R xs) = right R (remove' n xs)
