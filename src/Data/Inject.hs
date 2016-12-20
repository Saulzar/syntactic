
module Data.Inject where

import Data.Kind (Type)


class Project (sub :: k -> Type) (sup :: k -> Type)
  where

    -- | Partial projection from @sup@ to @sub@
    prj :: sup a -> Maybe (sub a)

class Project sub sup => sub :<: sup
  where
    -- | Injection from @sub@ to @sup@
    inj ::  sub sig -> sup sig
