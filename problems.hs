
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

-- Problem 6
pal [] = True
pal [x] = True
pal [x,y] = x == y
pal (x:xs)
  | x == last xs = pal (butlast xs)
  | otherwise =  False

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

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
      
    