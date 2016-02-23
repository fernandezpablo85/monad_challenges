{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

  import MCPrelude

  type Gen a = Seed -> (a, Seed)

  allRandsFor :: Seed -> [Integer]
  allRandsFor s = number : allRandsFor s2
    where (number, s2) = rand s

  allRandsForSeedOne :: [Integer]
  allRandsForSeedOne = (allRandsFor . mkSeed) 1

  fiveRands :: [Integer]
  fiveRands = take 5 allRandsForSeedOne

  randLetter :: Gen Char
  randLetter = generalA toLetter rand

  allStringsFor :: Seed -> String
  allStringsFor n = letter : allStringsFor seed
    where (letter, seed) = randLetter n

  randString3 :: String
  randString3 = take 3 allStringsForSeedOne
    where allStringsForSeedOne = allStringsFor $ mkSeed 1

  generalA :: (a -> b) -> Gen a -> Gen b
  generalA f g s = (f x, s')
    where (x, s') = g s

  randEven :: Gen Integer
  randEven = generalA (* 2) rand

  randOdd :: Gen Integer
  randOdd = generalA (+ 1) randEven

  randTen :: Gen Integer
  randTen = generalA (* 10) randOdd

  randPair :: Gen (Char, Integer)
  randPair s = ((c, i), s'')
    where (c, s') = randLetter s
          (i, s'') = rand s'

  generalPair :: Gen a -> Gen b -> Gen (a, b)
  generalPair a b s = ((ax, bx), s'')
    where (ax, s') = a s
          (bx, s'') = b s'

  randPair' :: Gen (Char, Integer)
  randPair' = generalPair2 randLetter rand

  generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
  generalB f a b s = (f ax bx, s'')
    where (ax, s') = a s
          (bx, s'') = b s'

  generalPair2 :: Gen a -> Gen b -> Gen (a, b)
  generalPair2 a b = generalB (\x y -> (x, y)) a b
