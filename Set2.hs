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


