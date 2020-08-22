-- skips :: [a] -> [[a]]
-- skips x


skipN :: int -> [a] -> [a]
skipN n lst = [snd x | x <- pair, ((fst x) `mod` n == 0)]
  where pair = zip [1..] lst
