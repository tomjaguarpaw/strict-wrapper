{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.List (intercalate)
import Data.Strict.Wrapper (strict, Strictly, pattern Strict)
import Control.Exception (pattern ErrorCall, try, evaluate)

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
  failed <- printTestTreeLefts =<< sequence shouldBeBottomTests
  case failed of
    True  -> error "Failure"
    False -> pure ()
