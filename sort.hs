quickSort::(Ord a)=> [a]->[a]
quickSort [] = []
quickSort(x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]


mergeSort::(Ord a)=> [a]->[a]
mergeSort lst = 
 if length lst <= 1 then lst
 else merge (mergeSort (take (div (length lst) 2) lst))
      (mergeSort (drop (div (length lst) 2) lst))
 where
  merge::(Ord a)=> [a]->[a]->[a]
  merge lst1 lst2 
   |null lst1 = lst2
   |null lst2 = lst1
   |(head lst1) < (head lst2) = (head lst1):(merge (tail lst1) lst2)
   |otherwise = (head lst2):(merge lst1 (tail lst2))



insertionSort::(Ord a)=> [a]->[a]
insertionSort = foldr insert [] 
 where
  insert::(Ord a)=> a->[a]->[a]
  insert x [] = [x]
  insert x (y:ys) = 
   if (y > x) then x:y:ys
   else y:(insert x ys)