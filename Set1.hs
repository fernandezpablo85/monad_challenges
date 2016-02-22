{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

  import MCPrelude

  allRandsFor :: Seed -> [Integer]
  allRandsFor s = number : allRandsFor s2
    where (number, s2) = rand s

  allRandsForSeedOne :: [Integer]
  allRandsForSeedOne = (allRandsFor . mkSeed) 1

  fiveRands :: [Integer]
  fiveRands = take 5 allRandsForSeedOne

  randLetter :: Seed -> (Char, Seed)
  randLetter s = (toLetter n, newSeed)
    where (n, newSeed) = rand s

  allStringsFor :: Seed -> String
  allStringsFor n = letter : allStringsFor seed
    where (letter, seed) = randLetter n

  randString3 :: String
  randString3 = take 3 allStringsForSeedOne
    where allStringsForSeedOne = allStringsFor $ mkSeed 1
