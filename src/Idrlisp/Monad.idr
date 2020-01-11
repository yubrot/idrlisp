module Idrlisp.Monad

import public Control.Catchable
import public Idrlisp.Error

%default total

public export
record LIO a where
  constructor MkLIO
  runLIO : IO (Either Error a)

export
Functor LIO where
  map f (MkLIO a) = MkLIO $ map (map f) a

export
Applicative LIO where
  pure = MkLIO . pure . Right
  (MkLIO f) <*> a = MkLIO $ do
    f <- f
    case f of
      Left err => pure $ Left err
      Right f => runLIO $ map f a

export
Monad LIO where
  (MkLIO a) >>= f = MkLIO $ do
    a <- a
    case a of
      Left err => pure $ Left err
      Right a => runLIO $ f a

export
Catchable LIO Error where
  throw = MkLIO . pure . Left
  catch (MkLIO a) f = MkLIO $ do
    a <- a
    case a of
      Left err => runLIO $ f err
      Right a => pure $ Right a

export
lift : IO a -> LIO a
lift x = MkLIO $ do
  x <- x
  pure $ Right x

