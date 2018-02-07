{-# LANGUAGE TupleSections #-}

module SymEither where

newtype SymEither a = SymEither (Bool, a)
  deriving (Eq, Show)

left :: SymEither a -> SymEither a
left (SymEither (_, a)) = SymEither (False, a)

toEither :: SymEither a -> Either a a
toEither (SymEither (False, a)) = Left a
toEither (SymEither (_, a))     = Right a

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
