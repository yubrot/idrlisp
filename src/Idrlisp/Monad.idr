module Idrlisp.Monad

import public Control.Catchable

%default total

public export
record LIO e a where
  constructor MkLIO
  runLIO : IO (Either e a)

export
Functor (LIO e) where
  map f (MkLIO a) = MkLIO $ map (map f) a

export
Applicative (LIO e) where
  pure = MkLIO . pure . Right
  (MkLIO f) <*> a = MkLIO $ do
    f <- f
    case f of
      Left err => pure $ Left err
      Right f => runLIO $ map f a

export
Monad (LIO e) where
  (MkLIO a) >>= f = MkLIO $ do
    a <- a
    case a of
      Left err => pure $ Left err
      Right a => runLIO $ f a

export
Catchable (LIO e) e where
  throw = MkLIO . pure . Left
  catch (MkLIO a) f = MkLIO $ do
    a <- a
    case a of
      Left err => runLIO $ f err
      Right a => pure $ Right a

export
lift : IO a -> LIO e a
lift x = MkLIO $ do
  x <- x
  pure $ Right x

