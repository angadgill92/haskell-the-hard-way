even' :: Integral a => a -> Bool
even' x = mod x 2 == 0

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

evenSum :: [Integer] -> Integer
evenSum l = accumSum 0 l

accumSum n l = if l == []
  then n
  else let x = head l
           xs = tail l
       in if even x
         then accumSum (n+x) xs
         else accumSum n xs
