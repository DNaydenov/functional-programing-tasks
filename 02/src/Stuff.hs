module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

group :: Eq a => [a] -> [[a]]
group [] = [] 
group (x:xs) = takeWhile (==x) (x:xs) : group (dropWhile (==x) xs) 

-- Not mandatory, delete if you don't want this.
--insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
--insertBy compare x= 

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = sortBy f (filter (\y -> (y `f` x)==LT)  xs) ++ [x] ++
                  sortBy f (filter (\y -> (y `f` x)/=LT)  xs)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = [] 
groupBy f (x:xs) = takeWhile (f x) (x:xs) :
                   groupBy f (dropWhile  (f x) xs) 
 
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = op (f x) (f y)  

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f 
  = map snd 
  . sortBy (compare `on` fst) 
  . map (f &&& id)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f 
  = map (map snd)
  . groupBy ((==) `on` fst)
--(\ x y -> (fst x) == (fst y)) 
  . map (f &&& id) 
--groupOn f xs = map (map snd ) ( groupBy (\ x y -> (fst x) == (fst y)) ( map (f &&& id) xs))
   

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f = groupOn f . sortOn f 
