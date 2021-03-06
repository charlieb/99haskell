import System.Random (random, randomR, randomRs, StdGen, mkStdGen, newStdGen)
import Data.List (delete, elemIndex,  permutations, tails, sortBy, groupBy)
import Data.Maybe (fromMaybe)

-- Problem 1
last' [] = error "No last element in zero length list"
last' [x] = x
last' (_:xs) = last' xs

-- Problem 2
lastbutone [] = error "Too few elements"
lastbutone [x] = error "Too few elements"
lastbutone [x,_] = x
lastbutone (_:xs) = lastbutone xs

-- Problem 3
kth (x:_) 0 = x
kth (_:xs) n
  | n < 0 = error "index less than zero"
  | n > 0 = kth xs (n-1)

-- Problem 4
len [] = 0
len (_:xs) = 1 + len xs

-- Problem 5
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

butlast [] = error "list too short"
butlast [x] = error "list too short"
butlast [x,_] = [x]
butlast (x:xs) = x:butlast xs

-- Problem 6 - palindrome
pal [] = True
pal [x] = True
pal [x,y] = x == y
pal (x:xs)
  | x == last xs = pal (butlast xs)
  | otherwise =  False

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatter (List []) = []
flatter (List (Elem x:xs)) = x:flatter (List xs)
flatter (List (List x:xs)) = flatter (List x) ++ flatter (List xs)

flatter2 (Elem x) = [x]
flatter2 (List xs) = foldr (++) [] $ map flatter2 xs

-- Problem 8
compr [] = []
compr [x] = [x]
compr (x:y:xs) | x == y = compr (x:xs)
compr (x:y:xs) | x /= y = [x] ++ compr (y:xs)

-- Problem 9
pack str = packr [] str
  where 
    packr acc [] = [acc]
    packr [] (x:xs) = packr [x] xs
    packr acc (x:xs) 
      | acc !! 0 == x = packr (x:acc) xs
      | acc !! 0 /= x = acc:packr [x] xs
      
    
-- Problem 10
rle1 xs = map (\ x -> (length x, head x)) (pack xs)
rle2 (lst:lsts) = reverse $ foldl rle2' [(1, lst)] lsts
  where
  rle2' ((n,x):xs) y
    | x == y = ((n+1, x):xs)
    | x /= y = ((1, y):(n, x):xs)

-- Problem 11
data RleElement a b = Multiple a b | Single b deriving (Show)
rle3 lst = map rle3' $ rle2 lst
  where 
  rle3' (n,x)
    | n == 1 = Single x
    | n > 1  = Multiple n x
  rle3'' (1,x) = Single x
  rle3'' (n,x) = Multiple n x

-- Problem 12
-- unrle lst = foldr (++) [] $ map unrle' lst
unrle = concatMap unrle'
  where
  unrle' (Multiple n x) = replicate n x
  unrle' (Single x) = [x]

-- Problem 13
-- (see rle2 from problem 10)
rle4 :: Eq a => [a] -> [(Int, a)]
rle4 = foldr rle4' []
  where
  rle4' x [] = [(1,x)] -- need the type definition at the top or haskell doesn't know the type of the [] here
  rle4' x ((n,y):xs) 
    | x == y = ((n+1, x):xs)
    | x /= y = ((1, y):(n, x):xs)

-- Problem 14
dupli = concatMap (\ x -> [x,x])

-- Problem 15
duplin lst n = concatMap (\ x -> replicate n x) lst
duplin2 lst n = concatMap (take n . repeat) lst

-- Problem 16
dropnth lst n =
  dropnth' lst n
  where
  dropnth' [] _ = []
  dropnth' (_:xs) 1 = dropnth' xs n
  dropnth' (x:xs) m = x:(dropnth' xs (m-1))

  -- interesting because it enumerates the list with a cycle, uses a filter on
  -- the enumeration, then uses map to remove the enumeration
dropnth2 xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

-- Problem 17
split lst n = 
  split' [] lst n
  where
  split' x y 0 = (x,y)
  split' xs (y:ys) n = split' (xs ++ [y]) ys (n-1)

split2 xs n = (drop n xs, take n xs)

-- Problem 18
slice lst n m = take (m - n +1) $ drop (n-1) lst

-- Problem 19
rotate xs n = 
  let tpl = split xs (if n >= 0 then n else (length xs) + n) in (snd tpl) ++ (fst tpl)

-- Problem 20
removeAt n xs = take (n-1) xs ++ drop n xs

  -- Interesting because it's sort of inside out like a prolog program.
  -- The result is declared first then instructions on how to construct it
  -- follow.
removeAt2 1 (x:xs) = (x, xs)
removeAt2 n (x:xs) = (l, x:r)
  where (l, r) = removeAt2 (n - 1) xs

-- Problem 21
insertAt x xs 0 = x:xs
insertAt _ [] _ = error "Insertion point beyond end of list"
insertAt y (x:xs) n = x:insertAt y xs (n-1)

insertAt2 x xs n = (take n xs) ++ [x] ++ (drop n xs)

-- Problem 22
range x y
  | x > y = error "Start of range is larger than end"
  | x == y = [y]
  | otherwise = x:range (succ x) y

  -- Makes a list of 1's of the right length then adds each one to the total of
  -- the last + the starting value
range1 l r = scanl (+) l (replicate (l - r) 1)

  -- Constructs an infinite list starting at x and going up by 1,
  -- takes the needed elements
range2 x y = take (y-x+1) $ iterate (+1) x

-- Problem 23
rnd_select range n =
  take n $ tail $ map sel $ scanl rnd_sel (random (mkStdGen 0)) [1..]
  where
  sel (x,_) = range !! x
  rnd_sel (_,gen) _ = randomR (0, (length range) -1) gen

  -- Simplified using randomRs instead
rnd_select2 range n =
  take n $ [range !! x | x <- randomRs (0, (length range) -1) (mkStdGen 0)]

  -- And if we want a different list each time then we have to use IO and do
rnd_select3 range n = do
  rng <- newStdGen
  return $ take n $ [range !! x | x <- randomRs (0, (length range) -1) rng]


-- Problem 24
range_select :: Int -> Int -> [Int]
range_select n m = rnd_select [1..m] n

-- Problem 25
rnd_permut lst = do
  seed <- newStdGen
  return $ rnd_permut' lst seed
  where rnd_permut' [] _ = []
        rnd_permut' xs rng = x:rnd_permut' remains rng'
          where x = xs !! pos
                remains = take pos xs ++ drop (pos+1) xs
                (pos, rng') = randomR (0, (length xs) -1) rng

rnd_permut1 lst = do
  seed <- newStdGen
  return $ perms !! (fst $ randomR (0, (length perms) -1) seed)
  where perms = permutations lst

-- Problem 26
  -- double recursion wtih two different termination conditions
  -- for the two seperate recursive branches
  -- I guess that's a rule for double recursion!
combs 1 xs = map (\ x -> [x]) xs
combs _ [] = []
combs n (x:xs) = (map (\ y -> x:y) $ combs (n-1) xs) ++ combs n xs
  -- map (x:) can be used instead of map (\ y -> x:y) because (x:) is a
  -- parially apllied functionk.

-- a half done distraction to try to avoid using mult_delete below
-- because elemIndex requires x to be (Eq x) and I'm not sure that's
-- really needed
combs2 1 xs = 
  map take_one $ take (length xs) $ iterate (1+) 0
  where 
    take_one n = (head back, front ++ tail back) 
      where 
        front = take n xs
        back = drop n xs

-- Problem 27
mult_delete :: (Eq t) => [t] -> [t] -> [t]
mult_delete xs ys = filter (\ x -> elemIndex x xs == Nothing) ys
group :: (Eq t) => Int -> [t] -> [([t],[t])]
group n xs = map (\ comb -> (comb, mult_delete comb xs)) $ combs n xs

group234 xs  
  | length xs /= 9 = error "Must have exactly 9 elements"
  | otherwise = 
    concatMap make_groups twos_threes_and_rest 
    where twos_and_rest = group 2 xs -- Get a list of all the possible sets of 2 and the remainders
            -- get all the possible combinations of 3 in each remainder, format:
            -- (two, [(three, remaining_four) ...])
          twos_threes_and_rest = map (\ (two, rest) -> (two, group 3 rest)) twos_and_rest
            -- convert the above format into a list of simple (two, three, four) tuples
          make_groups grp = map (\ (three, four) -> ((fst grp), three, four)) $ snd grp

-- groups "abcd" [1,2,1] -> [["a", "bc", "d"] ...]
-- groups [1,2,3,4] [1,2,1] -> [[[1], [2,3], [4]] ...]
groups :: (Eq t) => [t] -> [Int] -> [[[t]]]
  -- ask for no groups and you'll get no groups
groups _ [] = []
  -- ground condition is to get all the possible groups for single n and
  -- remove the remainders
groups xs [n] = map (\ (grp, _) -> [grp]) $ group n xs
  -- otherwise combine each group of n with all possible groups generated
  -- from the remainder and the remaining ns
groups xs (n:ns) = 
  concatMap (\ (grp, remainder) -> map (grp:) $ groups remainder ns) grps
  where grps = group n xs
  
-- Problem 28
lsort :: [[t]] -> [[t]]
lsort = sortBy (\ x y -> compare (length x) (length y))

alist_get :: (Eq t) => t -> [(t,s)] -> Maybe s
alist_get _ [] = Nothing
alist_get key ((k,v):xs) 
  | k == key = Just v
  | otherwise = alist_get key xs

alist_put :: (Eq t) => t -> s -> [(t,s)] -> [(t,s)]
alist_put k v [] = [(k,v)]
alist_put k v ((k',v'):xs)
  | k == k' = (k,v):xs
  | otherwise = (k',v'):alist_put k v xs

lfsort :: [[t]] -> [[t]]
lfsort xs =
  sortBy (\ x y -> compare (alist_get (length x) freqs) (alist_get (length y) freqs)) xs
  where
  freqs = foldl count_lengths [] $ map length xs :: [(Int, Int)]
    where 
      count_lengths alist len = alist_put len (1+val) alist
        where
          val = fromMaybe 0 $ alist_get len alist

  -- from the solutions page
  -- sorts the list into length order 
  -- groups the lists into equal length sub-lists (groupBy equalLength)
  -- uses lsort to sort them by length again
  -- concatenates all the sublists
  -- more elegant than my solution :(
lfsort1 :: [[a]] -> [[a]]
lfsort1 lists = concat groups
  where groups = lsort $ groupBy equalLength $ lsort lists
        equalLength xs ys = length xs == length ys


