module Lists where

last' :: [a] -> a
last' = last

lastButOne :: [a] -> a
lastButOne = last . init

length' :: [a] -> Int
length' = length

kth :: Int -> [a] -> a
kth = flip (!!)

reverse' :: [a] -> [a]
reverse' = reverse

palindrome :: (Eq a) => [a] -> Bool
palindrome = (==) <$> id <*> reverse

data NestedList a
    = Elem a
    | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) =
    let (first, remaining) = span (== x) xs
    in (x:first) : pack remaining

encode :: Eq a => [a] -> [(Int, a)]
encode = map ((,) <$> length <*> head) . pack

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

dupliN :: Int -> [a] -> [a]
dupliN n = concatMap (replicate 3)

dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n = map snd . filter ((/= 0) . (`mod` n) . fst) . zip [1..]

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' = splitAt

slice :: Int -> Int -> [a] -> [a]
slice start end = drop (start - 1) . take end

rotate :: Int -> [a] -> [a]
rotate n xs =
    rotate' n' xs
    where
        n' = if n < 0 then length xs + n else n
        rotate' n = (++) <$> drop n <*> take n
