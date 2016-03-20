{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

  import MCPrelude

  -- First.

  firstSeed :: Seed
  firstSeed = mkSeed 1

  randsForSeed :: Seed -> [Integer]
  randsForSeed seed = first : rest
    where (first, nextSeed) = rand seed
          rest = randsForSeed nextSeed

  fiveRands :: [Integer]
  fiveRands = take 5 $ randsForSeed firstSeed

  -- Second.

  randLetter :: Gen Char
  randLetter seed = (letter, seed')
    where (number, seed') = rand seed
          letter = toLetter number

  randLettersForSeed :: Seed -> [Char]
  randLettersForSeed seed = first : rest
    where (first, nextSeed) = randLetter seed
          rest = randLettersForSeed nextSeed

  randString3 :: [Char]
  randString3 = take 3 $ randLettersForSeed firstSeed

  -- Third.

  type Gen a = Seed -> (a, Seed)

  -- Without generalA

  --randEven :: Gen Integer
  --randEven seed = (even, seed')
  --  where (even, seed') = f $ rand seed
  --        f = (\x -> (2 * fst x, snd x))

  --randOdd :: Gen Integer
  --randOdd seed = (odd, seed')
  --  where (odd, seed') = f $ randEven seed
  --        f = (\x -> ( (succ . fst) x, snd x))

  --randTen :: Gen Integer
  --randTen seed = (tenth, seed')
  --  where (tenth, seed') = f $ rand seed
  --        f = (\x -> (10 * fst x, snd x))

  generalA :: (a -> b) -> Gen a -> Gen b
  generalA f gen seed = (ret, seed')
    where (first, seed') = gen seed
          ret = f first

  -- With generalA
  randEven :: Gen Integer
  randEven = generalA (\x -> x * 2) rand

  randOdd :: Gen Integer
  randOdd = generalA (\x -> x +1) randEven

  randTen :: Gen Integer
  randTen = generalA (\x -> x * 10) rand

  result :: Integer
  result = product [a, b, c]
    where (a, _) = randEven firstSeed
          (b, _) = randOdd firstSeed
          (c, _) = randTen firstSeed

  -- Fourth
  randPair :: Gen (Char, Integer)
  randPair seed = ((char, int), seed'')
    where (char, seed') = randLetter seed
          (int, seed'') = rand seed'

  generalPair :: Gen a -> Gen b -> Gen (a,b)
  generalPair gena genb seed = ((a, b), seed'')
    where (a, seed') = gena seed
          (b, seed'') = genb seed'

  generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
  generalB f gena genb seed = (f a b, seed'')
    where (a, seed') = gena seed
          (b, seed'') = genb seed'

  generalPair2 :: Gen a -> Gen b -> Gen (a,b)
  generalPair2 gena genb = generalB makeTuple gena genb
    where makeTuple = (\x y -> (x,y))