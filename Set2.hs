{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

  import MCPrelude

  -- First.
  data Maybe a = Nothing | Just a deriving(Ord,Eq)

  instance (Show a) => Show (Maybe a) where
    show (Just a) = "Just " ++ (show a)
    show Nothing = "Nothing"

  -- Second.
  headMay :: [a] -> Maybe a
  headMay [] = Nothing
  headMay (a:_) = Just a

  tailMay :: [a] -> Maybe [a]
  tailMay [] = Nothing
  tailMay (_:as) = Just as

  lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
  lookupMay _ [] = Nothing
  lookupMay a (t:ts) = if key == a then Just value else lookupMay a ts
    where key = fst t
          value = snd t

  divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
  divMay _ 0 = Nothing
  divMay a b = Just (a / b)

  maximumMay :: Ord a => [a] -> Maybe a
  maximumMay [] = Nothing
  maximumMay as = Just (foldl1 max as)

  minimumMay :: Ord a => [a] -> Maybe a
  minimumMay [] = Nothing
  minimumMay as = Just (foldl1 min as)

  -- Third.
  queryGreek :: GreekData -> String -> Maybe Double
  queryGreek gdata key = case lookupMay key gdata of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
      Nothing -> Nothing
      Just txs -> case maximumMay txs of
        Nothing -> Nothing
        Just mxs -> case headMay xs of
          Nothing -> Nothing
          Just hxs -> divMay (fromIntegral mxs) (fromIntegral hxs)

  -- Fourth.

  --chain :: (a -> Maybe b) -> Maybe a -> Maybe b
  --chain _ Nothing = Nothing
  --chain f (Just a) = f a

  link :: Maybe a -> (a -> Maybe b) -> Maybe b
  link Nothing _ = Nothing
  link (Just a) f = f a

  maxFromTail :: Ord a => [a] -> Maybe a
  maxFromTail xs = link tail maximumMay
    where tail = tailMay xs

  queryGreek2 :: GreekData -> String -> Maybe Double
  queryGreek2 gdata key = link pair (\(x,y) -> divMay y x)
    where pair = link head (\h -> link maxTail (\m -> Just (fromIntegral(h), fromIntegral(m))))
          maxTail = link tail maximumMay
          tail = link list tailMay
          head = link list headMay
          list = lookupMay key gdata

  -- Fifth.
  type Name = String

  ylink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  ylink f a b = link a (\a' -> link b (\b' -> mkMaybe (f a' b')))

  mkMaybe :: a -> Maybe a
  mkMaybe a = Just a

  tupled :: Maybe a -> Maybe b -> Maybe (a, b)
  tupled Nothing _ = Nothing
  tupled _ Nothing = Nothing
  tupled (Just a) (Just b) = mkMaybe (a, b)

  addSalaries :: [(Name, Integer)] -> Name -> Name -> Maybe Integer
  addSalaries ss name1 name2 = ylink (+) salary1 salary2
    where salary1 = lookupMay name1 ss
          salary2 = lookupMay name2 ss