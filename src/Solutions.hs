module Solutions
  where

-- Problem 1
lastBox :: [a] -> [a]
lastBox []     = []
lastBox xs@[x] = xs
lastBox (x:xs) = lastBox xs

-- Problem 2
lastButOne :: [a] -> [a]
lastButOne []        = []
lastButOne (x:[])    = []
lastButOne xs@[x, y] = xs
lastButOne (x:xs)    = lastButOne xs

-- Problem 3
kth :: Int -> [a] -> a
kth _ [] = error "kth of empty list"
kth 1 (x:xs) = x
kth n (_:xs) = kth (n-1) xs

-- Problem 4
len :: [a] -> Int
len xs =
  go 0 xs
  where
    go :: Int -> [a] -> Int
    go i []     = i
    go i (_:xs) = go (i + 1) xs

-- Problem 5
rev :: (Eq a) => [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == rev xs

--Problem 7
flat :: [[a]] -> [a]
flat [] = []
flat (xs:ys) = xs ++ flat ys

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress (a:b:xs) =
  if a == b
  then compress (a:xs)
  else a:(compress (b:xs))
compress xs = xs

-- Problem 8
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = pack' (tail xs) [] ([head xs])
  where
    pack' :: (Eq a) => [a] -> [[a]] -> [a] -> [[a]]
    pack' [] acc _ = rev acc
    pack' (x:xs) acc (y:ys) =
      if x == y
      then pack' xs acc (x:y:ys)
      else pack' xs ((y:ys):acc) [x]

-- Problem 9
runLength :: (Eq a) => [a] -> [(Int, a)]
runLength [] = []
runLength xs =
  map' (\ l -> (len l, head l)) $ pack xs

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = (f x):(map f xs)

-- Problem 10

-- This one is too tough for Haskell and we're not sure whether it's
-- really useful in Haskell anyway.

-- Problem 11
decompress :: (Eq a) => [(Int, a)] -> [a]
decompress [] = []
decompress ((count, el):rest) =
  replicate' count el ++ decompress rest

replicate' :: Int -> a -> [a]
replicate' i el =
  if i < 1
  then []
  else el:(replicate' (i - 1) el)

-- Problem 12
runLength' :: (Eq a) => [a] -> [(Int, a)]
runLength' [] = []
runLength' xs = runLength'' (tail xs) [] 1 (head xs)
  where
    runLength'' :: (Eq a) => [a] -> [(Int, a)] -> Int -> a -> [(Int, a)]
    runLength'' []     acc count _ = rev acc
    runLength'' (x:xs) acc count z =
      if x == z
      then runLength'' xs acc              (count + 1) z
      else runLength'' xs ((count, z):acc) 1           x

-- Problem 13
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:(dupli xs)
