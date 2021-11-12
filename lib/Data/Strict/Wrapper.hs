{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Strict.Wrapper
  (
  -- * Introduction

  -- ** Background

  -- | To avoid space leaks it is important to ensure that strictness
  -- annotations are inserted appropriately.  For example, instead of
  -- writing
  --
  -- @
  -- pairFoldBad :: (Integer, Integer)
  -- pairFoldBad = foldl' f (0, 0) [1..million]
  --  where f (count, theSum) x = (count + 1, theSum + x)
  -- @
  --
  -- we could write
  --
  -- @
  -- pairFoldBangs :: (Integer, Integer)
  -- pairFoldBangs = foldl' f (0, 0) [1..million]
  --  where f (!count, !theSum) x = (count + 1, theSum + x)
  -- @
  --
  -- The downside of avoiding the space leak by inserting those bang
  -- patterns is that we have to remember to do so.  Nothing in the
  -- types guides us to insert them.  One way of addressing that
  -- problem is to define a type of "strict pairs" and use it instead
  -- of Haskell's built-in (lazy) pair.
  --
  -- @
  -- data StrictPair a b = StrictPair !a !b
  --
  -- pairFoldStrictPair :: StrictPair Integer Integer
  -- pairFoldStrictPair = foldl' f (StrictPair 0 0) [1..million]
  --  where f (StrictPair count theSum) x = StrictPair (count + 1) (theSum + x)
  -- @
  --
  -- The strictness annotations on the fields of the @StrictPair@
  -- constructor cause the compiler to evaluate the fields before the
  -- pair is constructed.  The syntax above desugars to the form
  -- below:
  --
  -- @
  -- pairFoldStrictPair_Desugared :: StrictPair Integer Integer
  -- pairFoldStrictPair_Desugared = foldl' f (StrictPair 0 0) [1..million]
  --  where f (StrictPair count theSum) x = let !count'  = count + 1
  --                                            !theSum' = theSum + x
  --                                        in StrictPair count' theSum'
  -- @
  --
  -- (@pairFoldStrictPair_Desugared@ forces the fields at construction
  -- time and @pairFoldBangs@ forces the fields when the pair is
  -- pattern matched but the consequences are the same: the space leak
  -- is avoided.)
  --
  -- Using `StrictPair` is helpful because we can't forget to evaluate
  -- the components.  It happens automatically.
  --
  -- If we take the "define strict data types" approach to solving
  -- space leaks then we need a strict version of every basic data
  -- type.  For example, to fix the space leak in the following:
  --
  -- @
  -- maybeFoldBad :: (Integer, Maybe Integer)
  -- maybeFoldBad = foldl' f (0, Nothing) [1..million]
  --  where f (i, Nothing) x = (i + 1, Just x)
  --        f (i, Just j)  x = (i + 2, Just (j + x))
  -- @
  --
  -- we need to define @StrictMaybe@ and use it as below:
  --
  -- @
  -- data StrictMaybe a = StrictNothing | StrictJust !a
  --
  -- maybeFoldStrictMaybe :: StrictPair Integer (StrictMaybe Integer)
  -- maybeFoldStrictMaybe = foldl' f (StrictPair 0 StrictNothing) [1..million]
  --  where f (StrictPair i StrictNothing)  x = StrictPair (i + 1) (StrictJust x)
  --        f (StrictPair i (StrictJust j)) x = StrictPair (i + 2) (StrictJust (j + x))
  -- @
  --
  -- The "define strict data types" approach requires a whole
  -- "parallel universe" of strict versions of basic types and is
  -- likely to become very tedious very quickly.
  -- (<https://hackage.haskell.org/package/strict strict> is one
  -- library providing such functionality.)

  -- ** @strict-wrapper@

  -- | @strict-wrapper@ provides a convenient way of using strict
  -- versions of basic data types without requiring a strict "parallel
  -- universe".  It provides a data family t'Strict' that maps basic
  -- types to their strict versions
  --
  -- @
  -- data instance t'Strict' (a, b)    = StrictPair !a !b
  -- data instance t'Strict' (Maybe a) = StrictNothing | StrictJust !a
  -- ...
  -- @
  --
  -- and a bidirectional pattern synonym, also called v'Strict', for
  -- mapping between the lazy and strict versions.  By using
  -- @strict-wrapper@ the example above, @maybeFoldStrictMaybe@, can
  -- be written as
  --
  -- @
  -- maybeFoldStrict :: Strict (Integer, Strict (Maybe Integer))
  -- maybeFoldStrict = foldl' f (strict (0, Strict Nothing)) [1..million]
  --  where f (Strict (i, Strict Nothing))  x = Strict (i + 1, Strict (Just x))
  --        f (Strict (i, Strict (Just j))) x = Strict (i + 2, Strict (Just (j + x)))
  -- @
  --
  -- When using @strict-wrapper@ there is no need to have a parallel
  -- universe of strict types with new names that we must remember
  -- (@StrictPair@, @StrictMaybe@, @StrictJust@, @StrictNothing@,
  -- ...).  All that we need to do is to insert the v'Strict'
  -- constructor or pattern in the places that we are guided to do so
  -- by the type checker.

  -- *** Nested strict data

  -- | It is common in the Haskell world to see strict data field
  -- definitions like
  --
  -- @
  -- data MyData = MyData { field1 :: !(Maybe Bool)
  --                      , field2 :: !(Either (Int, Double) Float)
  --                      }
  -- @
  --
  -- Those strict fields probably don't do what the author hoped!
  -- They are almost entirely pointless.  The bang annotations on the
  -- @Maybe@ ensure only that is is evaluated to a @Nothing@ or
  -- @Just@.  The @Bool@ is left unevaluated.  Similarly the @Either@
  -- is evaluated only as far as a @Left@ or @Right@.  The pair and
  -- @Float@ inside are left unevaluated.  @strict-wrapper@ can help
  -- here.  Wrap both the @Maybe@ and the pair in @Strict@ and the
  -- type becomes fully strict!
  --
  -- @
  -- data MyDataStrict = MyDataStrict { field1 :: !(Strict (Maybe Bool))
  --                                  , field2 :: !(Strict (Either (Strict (Int, Double)) Float))
  --                                  }
  -- @

  -- ** The API

  -- | To use @strict-wrapper@ all that you need is the data family
  -- t'Strict' and the bidirectional pattern synonym v'Strict'.  For
  -- example, instead of using @StrictPair a b@ as defined above, use
  -- @Strict (a, b)@.  To create a @Strict (a, b)@ wrap an @(a, b)@ in
  -- the v'Strict' constructor; to extract an @(a, b)@, pattern match
  -- with t'Strict'.

  -- ** Efficiency considerations

  -- | Using @strict-wrapper@ should be zero-cost relative to inserting
  -- 'seq' or bang patterns manually.  In some cases matching the
  -- baseline cost will require using the functions 'strict' and
  -- 'unstrict'.  They provide the same functionality as the v'Strict'
  -- pattern/constructor synonym but can be more efficient in
  -- particular circumstances. We suggest just using v'Strict' until
  -- and unless you find a performance problem.

  -- ** Further reading

  -- | You can read <http://h2.jaguarpaw.co.uk/posts/nested-strict-data/ the blog post>
  -- by Tom Ellis where the design of this library was first proposed.

  -- * Strict constructor and pattern

  -- | The @Strict@ constructor and pattern are the easiest way to get
  -- started with @strict-wrapper@.
  pattern Strict

  -- * Types that have a strict version

  , Strict

  -- * Accessor functions

  -- | The accessor functions can be more efficient than the v'Strict'
  -- constructor and pattern in some circumstances but we don't
  -- recommend that you use them unless you are experiencing
  -- performance problems.

  , strict
  , unstrict
  -- * Class
  , Strictly(matchStrict, constructStrict)
  -- * Error messages

  -- | These diagnostic error messages can appear when you try to use
  -- @Strict@ on a type that doesn't support it.
  , AlreadyStrict
  , CannotBeStrict
  , NestedStrict
  , NotYetImplemented
  ) where

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits
import Data.Kind (Constraint)

-- WARNING: For 'strict' and 'unstrict' to have zero run time cost
-- they are implemented in terms of 'unsafeCoerce'.  This will lead to
-- CATASTROPHIC BREAKAGE unless you are very careful to ensure that
-- 'Strict a' has the same run time representation as a
-- fully-evaulated 'a', and that when mapping from 'a' to 'Strict a'
-- you ensure all its fields are fully evaluated.

-- | A type @t@ can be given a @Strictly@ instance when it has a very
-- cheap conversion to and from a strict type, @Strict t@.
class Strictly t where
  -- | Isomorphic to the type @t@, except that when it is evaulated its
  -- immediate children are evaluated too.
  data Strict t
  -- | Make a @Strict t@ using 'strict' if you obtained a whole @t@
  -- from elsewhere (otherwise, if you have the components of @t@
  -- separately, then it is more efficient to use the v'Strict'
  -- constructor instead).
  --
  -- @
  -- makeStrict :: (Int, Strict (Int, String)) -> Int
  -- makeStrict (i, s) = i + f (strict s)
  -- @
  strict :: t -> Strict t
  -- | Access the contents of a @Strict t@, but not its fields, using
  -- @unstrict@ (if you want access to the fields then it is more
  -- efficient to use the v'Strict' pattern).
  --
  -- @
  -- strictMaybe :: r -> (a -> r) -> Strict (Maybe a) -> r
  -- strictMaybe r f sm = maybe r f (unstrict sm)
  -- @
  unstrict :: Strict t -> t
  -- | Used to implement the v'Strict' pattern synonym.  You should
  -- never need to use @matchStrict@ unless you are defining your own
  -- instance of @Strictly@.
  matchStrict :: Strict t -> t
  -- | Used to implement the v'Strict' constructor.  You should never
  -- need to use @constructStrict@ unless you are defining your own
  -- instance of @Strictly@.
  constructStrict :: t -> Strict t

instance Strictly (t1, t2) where
  data Strict (t1, t2) = StrictPair !t1 !t2
  strict x = unsafeCoerce $ case x of
    (!_, !_) -> x
  matchStrict = \case
    StrictPair t1 t2 -> (t1, t2)
  unstrict = unsafeCoerce
  constructStrict (x, y) = StrictPair x y

instance Strictly (t1, t2, t3) where
  data Strict (t1, t2, t3) = StrictT3 !t1 !t2 !t3
  strict x = unsafeCoerce $ case x of
    (!_, !_, !_) -> x
  matchStrict = \case
    StrictT3 x1 x2 x3 -> (x1, x2, x3)
  unstrict = unsafeCoerce
  constructStrict (x1, x2, x3) = StrictT3 x1 x2 x3

instance Strictly (t1, t2, t3, t4) where
  data Strict (t1, t2, t3, t4) = StrictT4 !t1 !t2 !t3 !t4
  strict x = unsafeCoerce $ case x of
    (!_, !_, !_, !_) -> x
  matchStrict = \case
    StrictT4 x1 x2 x3 x4 -> (x1, x2, x3, x4)
  unstrict = unsafeCoerce
  constructStrict (x1, x2, x3, x4) = StrictT4 x1 x2 x3 x4

instance Strictly (Maybe t) where
  data Strict (Maybe t) = StrictNothing | StrictJust !t
  strict x = unsafeCoerce $ case x of
    Nothing -> x
    Just !_ -> x
  matchStrict = \case
    StrictJust j  -> Just j
    StrictNothing -> Nothing
  unstrict = unsafeCoerce
  constructStrict = \case
    Just j  -> StrictJust j
    Nothing -> StrictNothing

instance Strictly (Either t1 t2) where
  data Strict (Either t1 t2) = StrictLeft !t1 | StrictRight !t2
  strict x = unsafeCoerce $ case x of
    Left !_  -> x
    Right !_ -> x
  matchStrict = \case
    StrictLeft l  -> Left l
    StrictRight r -> Right r
  unstrict = unsafeCoerce
  constructStrict = \case
    Left l  -> StrictLeft l
    Right r -> StrictRight r

-- | Some data types, such as 'Int' and 'Double', are already as
-- strict as they can be.  There is no need to wrap them in t'Strict'!
type family AlreadyStrict t :: Constraint
type instance AlreadyStrict t =
  TypeError (('ShowType t ':<>: 'Text " is already strict.")
              ':$$: ('Text "Just use "
                     ':<>: 'ShowType t
                     ':<>: 'Text " rather than Strict ("
                     ':<>: 'ShowType t
                     ':<>: 'Text ")"))

-- | Some data types, such as @[a]@, can't be made strict in a
-- zero-cost way.
type family CannotBeStrict t :: Constraint
type instance CannotBeStrict t =
  TypeError ('ShowType t ':<>: 'Text " can't be made strict.")

-- | Some 'Strictly' instances are not yet implemented.  Please file
-- an issue if you need them.
type family NotYetImplemented t :: Constraint
type instance NotYetImplemented t =
  TypeError ('Text "Strict is not yet implemented for " ':<>: 'ShowType t
             ':$$: 'Text "Please file an issue if you need it")

-- | It is redundant to nest t'Strict', e.g. @Strict (Strict (t1, t2))@.
-- Just use one layer of t'Strict'.
type family NestedStrict t :: Constraint
type instance NestedStrict t =
  TypeError ('Text "It is redundant to nest Strict"
             ':$$: 'Text "In type Strict (Strict (" ':<>: 'ShowType t ':<>: 'Text "))"
             ':$$: 'Text "Just use Strict (" ':<>: 'ShowType t ':<>: 'Text ") instead")

instance AlreadyStrict () => Strictly ()
instance AlreadyStrict Bool => Strictly Bool
instance AlreadyStrict Int => Strictly Int
instance AlreadyStrict Integer => Strictly Integer
instance AlreadyStrict Float => Strictly Float
instance AlreadyStrict Double => Strictly Double
instance AlreadyStrict Word => Strictly Word
instance AlreadyStrict Ordering => Strictly Ordering
instance AlreadyStrict Char => Strictly Char

instance CannotBeStrict [t] => Strictly [t]
instance CannotBeStrict (IO a) => Strictly (IO a)

instance NotYetImplemented (x1, x2, x3, x4, x5) => Strictly (x1, x2, x3, x4, x5)
instance NotYetImplemented (x1, x2, x3, x4, x5, x6) => Strictly (x1, x2, x3, x4, x5, x6)

instance NestedStrict t => Strictly (Strict t)

-- | Use the @Strict@ pattern if you want to subsequently match on the
--  @t@ it contains (otherwise it is more efficient to use 'strict').
--
-- @
-- printIt :: Strict (Maybe Int) -> IO ()
-- printIt (Strict (Just i)) = print i
-- printIt (Strict Nothing)  = putStrLn "Nothing there"
-- @
--
-- Make a @Strict t@ using the @Strict@ constructor if you are
-- constructing it from its individual fields (otherwise it is more
-- efficient to use 'unstrict').
--
-- @
-- makeStrict :: Int -> Strict (Int, String)
-- makeStrict i = Strict (i + 1, show i)
-- @
pattern Strict :: Strictly t => t -> Strict t
pattern Strict x <- (matchStrict->x)
  where Strict = constructStrict

{-# COMPLETE Strict :: Strict #-}
