{-|
This module provides a symmetrical "Either" type.

This is roughly the equivalent of @Either a a@, but doesn't treat
'Left' vs. 'Right' as special. e.g., @fmap@ maintains left- or
right-ness while allowing value transformation.
-}

{-# LANGUAGE TupleSections #-}
module SymEither ( SymEither(..)
                 , left, toEither, fromEither
                 ) where

-- | @SymEither a@ is similar to @Either a a@.  The "Bool" value is
-- "True" for a "SymEither" that represents a "Right" value.
newtype SymEither a = SymEither (Bool, a)
  deriving (Eq, Show)

-- | Ensure the value is on the left side.
left :: SymEither a -> SymEither a
left (SymEither (_, a)) = SymEither (False, a)

-- | Convert a SymEither to a standard "Either".
toEither :: SymEither a -> Either a a
toEither (SymEither (False, a)) = Left a
toEither (SymEither (_, a))     = Right a

-- | Convert from an Either type to a SymEither.
fromEither :: b -> Either a b -> SymEither b
fromEither a (Left _) = SymEither (False, a)
fromEither _ (Right x) = SymEither (True, x)

instance Functor SymEither where
  fmap f (SymEither x) = (SymEither . fmap f) x

cf :: Bool -> SymEither a -> SymEither a
cf True = id
cf False = left

instance Applicative SymEither where
  pure                    = SymEither . (True,)
  (SymEither (t,f)) <*> r = (cf t.fmap f) r

instance Monad SymEither where
  (SymEither (t,l)) >>= k = (cf t.k) l
