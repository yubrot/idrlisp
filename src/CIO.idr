module CIO

import public Control.Catchable

%default total

||| Catchable IO.
public export
record CIO e a where
  constructor MkCIO
  runCIO : IO (Either e a)

export
Functor (CIO e) where
  map f (MkCIO a) = MkCIO $ map (map f) a

export
Applicative (CIO e) where
  pure = MkCIO . pure . Right
  (MkCIO f) <*> a = MkCIO $ do
    f <- f
    case f of
      Left err => pure $ Left err
      Right f => runCIO $ map f a

export
Monad (CIO e) where
  (MkCIO a) >>= f = MkCIO $ do
    a <- a
    case a of
      Left err => pure $ Left err
      Right a => runCIO $ f a

export
Catchable (CIO e) e where
  throw = MkCIO . pure . Left
  catch (MkCIO a) f = MkCIO $ do
    a <- a
    case a of
      Left err => runCIO $ f err
      Right a => pure $ Right a

export
lift : IO a -> CIO e a
lift x = MkCIO $ do
  x <- x
  pure $ Right x

