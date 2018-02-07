module SymEither where

data SymEither a = SLeft a | SRight a
  deriving (Eq, Show)

left :: SymEither a -> SymEither a
left l@(SLeft _) = l
left (SRight x)  = SLeft x

toEither :: SymEither a -> Either a a
toEither (SLeft a)  = Left a
toEither (SRight a) = Right a

fromEither :: b -> Either a b -> SymEither b
fromEither a (Left _) = SLeft a
fromEither _ (Right x) = SRight x

instance Functor SymEither where
  fmap f (SLeft a)  = SLeft (f a)
  fmap f (SRight a) = SRight (f a)

instance Applicative SymEither where
  pure           = SRight
  SLeft  f <*> r = (left.fmap f) r
  SRight f <*> r = fmap f r

instance Monad SymEither where
  SLeft  l >>= k = (left.k) l
  SRight r >>= k = k r
