module Idrlisp.Vec

import Data.IOArray

%default total

||| An abstract representation of idrlisp vectors.
export
data Vec a
  = NonEmpty (IOArray a) Int
  | Empty

export
length : Vec a -> Nat
length Empty = 0
length (NonEmpty _ l) = cast l

export
empty : Vec a
empty = Empty

export
new : Nat -> a -> IO (Vec a)
new size zero = do
  array <- newArray (cast size) zero
  pure $ NonEmpty array (cast size)

export
fromList : List a -> IO (Vec a)
fromList [] = pure empty
fromList (x :: xs) = do
  let l : Int = cast (length xs)
  array <- newArray (1 + l) x
  sequence_ $ zipWith (unsafeWriteArray array) [1,2..l] xs
  pure $ NonEmpty array (1 + l)

export
toList : Vec a -> IO (List a)
toList Empty = pure []
toList (NonEmpty arr l) = traverse (unsafeReadArray arr) [0,1..l-1]

export
read : Nat -> Vec a -> IO (Maybe a)
read index Empty = pure Nothing
read index (NonEmpty arr l) =
  if l <= cast index
    then pure Nothing
    else Just <$> unsafeReadArray arr (cast index)

export
write : Nat -> a -> Vec a -> IO Bool
write index value Empty = pure False
write index value (NonEmpty arr l) =
  if l <= cast index
    then pure False
    else unsafeWriteArray arr (cast index) value *> pure True

export
copy : Vec a -> Nat -> Vec a -> Nat -> Nat -> IO Bool
copy src srcStart dest destStart Z =
  pure $ srcStart <= length src && destStart <= length dest
copy (NonEmpty src srcLen) srcStart (NonEmpty dest destLen) destStart (S len) =
  if srcStart + S len <= cast srcLen && destStart + S len <= cast destLen
    then traverse readWrite [0,1..len] *> pure True
    else pure False
  where
    readWrite : Nat -> IO ()
    readWrite i =
      unsafeReadArray src (cast (i + srcStart)) >>=
      unsafeWriteArray dest (cast (i + destStart))
copy src srcStart dest destStart len = pure False

