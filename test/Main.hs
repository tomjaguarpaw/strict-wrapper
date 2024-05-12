{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (intercalate)
import Data.Strict.Wrapper
  (constructStrict,
   matchStrict,
   strict,
   unstrict,
   Strictly,
   Strict,
   pattern Strict
  )
import Control.Exception (pattern ErrorCall, try, evaluate)
import System.Exit (exitFailure)

bottom :: a
bottom = error bottomS

bottomS :: String
bottomS = "bottom"

shouldBeBottomG :: (t -> t') -> Bool -> t -> IO (Either String ())
shouldBeBottomG strict_ shouldBeBottom_ x = do
  resultOrError <- try (evaluate (strict_ x))

  let wasBottomE = case resultOrError of
        Left (ErrorCall e) -> case e == bottomS of
          True  -> Right True
          False -> Left e
        Right _result -> Right False

  pure $ case wasBottomE of
    Left unexpectedError -> Left ("Got an unexpected error: " <> unexpectedError)
    Right wasBottom -> case (shouldBeBottom_, wasBottom) of
      (True, True)   -> Right ()
      (False, False) -> Right ()
      (True, False)  -> Left "Expected bottom but got no error"
      (False, True)  -> Left "Got unexpected bottom"

data TestTree a = Leaf a
                | TestTree [(String, TestTree a)]
  deriving (Functor, Foldable, Traversable)

shouldBeBottom :: Strictly t
               => Bool
               -> t
               -> TestTree (IO (Either String ()))
shouldBeBottom shouldBe x =
  TestTree [ ("Strict (constructor)", Leaf (shouldBeBottomG Strict shouldBe x))
           , ("strict (accessor)",    Leaf (shouldBeBottomG strict shouldBe x)) ]

shouldBeBottomTests :: TestTree (IO (Either String ()))
shouldBeBottomTests =
  TestTree
  [ ("Nothing",  shouldBeBottom False Nothing)
  , ("Just ()",  shouldBeBottom False (Just ()))
  , ("Just !",   shouldBeBottom True  (Just __))

  , ("Left ()",  shouldBeBottom False (Left ()))
  , ("Left !",   shouldBeBottom True  (Left __))
  , ("Right ()", shouldBeBottom False (Right ()))
  , ("Right !",  shouldBeBottom True  (Right __))

  , ("(,)",      shouldBeBottom False ((), ()))
  , ("(!,)",     shouldBeBottom True  (__, ()))
  , ("(,!)",     shouldBeBottom True  ((), __))
  , ("(!,!)",    shouldBeBottom True  (__, __))

  , ("(,,)",     shouldBeBottom False ((), (), ()))
  , ("(!,,)",    shouldBeBottom True  (__, (), ()))
  , ("(,!,)",    shouldBeBottom True  ((), __, ()))
  , ("(!,!,)",   shouldBeBottom True  (__, __, ()))
  , ("(,,!)",    shouldBeBottom True  ((), (), __))
  , ("(!,,!)",   shouldBeBottom True  (__, (), __))
  , ("(,!,!)",   shouldBeBottom True  ((), __, __))
  , ("(!,!,!)",  shouldBeBottom True  (__, __, __))

  , ("(,,,)",     shouldBeBottom False ((), (), (), ()))
  , ("(!,,,)",    shouldBeBottom True  (__, (), (), ()))
  , ("(,!,,)",    shouldBeBottom True  ((), __, (), ()))
  , ("(!,!,,)",   shouldBeBottom True  (__, __, (), ()))
  , ("(,,!,)",    shouldBeBottom True  ((), (), __, ()))
  , ("(!,,!,)",   shouldBeBottom True  (__, (), __, ()))
  , ("(,!,!,)",   shouldBeBottom True  ((), __, __, ()))
  , ("(!,!,!,)",  shouldBeBottom True  (__, __, __, ()))
  , ("(,,,!)",    shouldBeBottom True  ((), (), (), __))
  , ("(!,,,!)",   shouldBeBottom True  (__, (), (), __))
  , ("(,!,,!)",   shouldBeBottom True  ((), __, (), __))
  , ("(!,!,,!)",  shouldBeBottom True  (__, __, (), __))
  , ("(,,!,!)",   shouldBeBottom True  ((), (), __, __))
  , ("(!,,!,!)",  shouldBeBottom True  (__, (), __, __))
  , ("(,!,!,!)",  shouldBeBottom True  ((), __, __, __))
  , ("(!,!,!,!)", shouldBeBottom True  (__, __, __, __))
  ]
  where __ = bottom

strictEqualsConstructStrict ::
  (Strictly t, Eq (Strict t)) => t -> TestTree (IO (Either String ()))
strictEqualsConstructStrict a =
  Leaf $ pure $
    if strict a == constructStrict a
      then pure ()
      else Left "Failed"

unstrictInverse :: (Eq a, Strictly a) => a -> TestTree (IO (Either String ()))
unstrictInverse a =
  Leaf $ pure $
    if a == (unstrict . strict) a
      then pure ()
      else Left "Failed"

matchStrictInverse :: (Eq a, Strictly a) => a -> TestTree (IO (Either String ()))
matchStrictInverse a =
  Leaf $ pure $
    if a == (matchStrict . strict) a
      then pure ()
      else Left "Failed"

valueTests :: TestTree (IO (Either String ()))
valueTests =
  TestTree
    [("strict equals constructStrict",
      TestTree [
         ("Left", strictEqualsConstructStrict @(Either Int Int) (Left 1)),
         ("Right", strictEqualsConstructStrict @(Either Int Int) (Right 1)),
         ("Nothing", strictEqualsConstructStrict @(Maybe Int) Nothing),
         ("Just", strictEqualsConstructStrict @(Maybe Int) (Just 1)),
         ("Tuple", strictEqualsConstructStrict @(Int, Int) (1, 2))
         ]),
     ("unstrict is inverse of strict",
      TestTree [
         ("Left", unstrictInverse @(Either Int Int) (Left 1)),
         ("Right", unstrictInverse @(Either Int Int) (Right 1)),
         ("Nothing", unstrictInverse @(Maybe Int) Nothing),
         ("Just", unstrictInverse @(Maybe Int) (Just 1)),
         ("Tuple", unstrictInverse @(Int, Int) (1, 2))
         ]),
     ("matchStrict is inverse of strict",
      TestTree [
         ("Left", matchStrictInverse @(Either Int Int) (Left 1)),
         ("Right", matchStrictInverse @(Either Int Int) (Right 1)),
         ("Nothing", matchStrictInverse @(Maybe Int) Nothing),
         ("Just", matchStrictInverse @(Maybe Int) (Just 1)),
         ("Tuple", matchStrictInverse @(Int, Int) (1, 2))
         ])
    ]

ordEquivalent ::
  (Ord a, Ord (Strict a), Strictly a) =>
  a ->
  a ->
  TestTree (IO (Either String ()))
ordEquivalent a1 a2 =
  Leaf $ pure $
    if compare a1 a2 == compare (strict a1) (strict a2)
      then pure ()
      else Left "Failed"

ordTests :: TestTree (IO (Either String ()))
ordTests =
  TestTree
    [("Ord tests",
      TestTree [
         -- Maybe
         ("Nothing, Nothing", ordEquivalent @(Maybe Int) Nothing Nothing),
         ("Just 1, Just 2", ordEquivalent @(Maybe Int) (Just 1) (Just 2)),
         ("Just 2, Just 1", ordEquivalent @(Maybe Int) (Just 2) (Just 1)),
         ("Just 2, Nothing", ordEquivalent @(Maybe Int) (Just 2) Nothing),
         ("Nothing, Just 1", ordEquivalent @(Maybe Int) Nothing (Just 1)),
         -- Tuple
         ("(1, 10), (1, 2)", ordEquivalent @(Int, Int) (1, 10) (1, 2)),
         ("(1, 2), (1, 10)", ordEquivalent @(Int, Int) (1, 2) (1, 10)),
         ("(10, 1), (2, 1)", ordEquivalent @(Int, Int) (10, 1) (2, 1)),
         ("(2, 1), (10, 1)", ordEquivalent @(Int, Int) (2, 1) (10, 1)),
         -- Either
         ("Left 1, Left 2", ordEquivalent @(Either Int Int) (Left 1) (Left 2)),
         ("Left 2, Left 1", ordEquivalent @(Either Int Int) (Left 2) (Left 1)),
         ("Right 1, Right 2", ordEquivalent @(Either Int Int) (Right 1) (Right 2)),
         ("Right 2, Right 1", ordEquivalent @(Either Int Int) (Right 2) (Right 1)),
         ("Left 1, Right 1", ordEquivalent @(Either Int Int) (Left 1) (Right 1)),
         ("Right 1, Left 1", ordEquivalent @(Either Int Int) (Right 1) (Left 1))
      ])]

printTestTreeLefts :: TestTree (Either String a) -> IO Bool
printTestTreeLefts = errorOnTestTreeLeftsPrefix []
  where errorOnTestTreeLeftsPrefix :: [String]
                                   -> TestTree (Either String a)
                                   -> IO Bool
        errorOnTestTreeLeftsPrefix prefix = \case
          Leaf (Left e) -> do
            putStrLn (intercalate ": " (reverse (e:prefix)))
            pure True
          Leaf (Right _) ->
            pure False
          TestTree xs ->
            errorTree prefix xs

        errorTree :: [String]
                  -> [(String, TestTree (Either String a))]
                  -> IO Bool
        errorTree prefix = \case
          []   -> pure False
          (prefix', x):xs -> do
            failed <- errorOnTestTreeLeftsPrefix (prefix':prefix) x
            failedRest <- errorTree prefix xs
            pure (failed || failedRest)

main :: IO ()
main = do
  failed1 <- printTestTreeLefts =<< sequence shouldBeBottomTests
  failed2 <- printTestTreeLefts =<< sequence valueTests
  failed3 <- printTestTreeLefts =<< sequence ordTests
  case failed1 || failed2 || failed3 of
    True  -> do
      putStrLn "Failure"
      exitFailure
    False -> pure ()
