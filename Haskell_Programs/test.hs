-- Writing Haskell functions is very easy!
-- They are as simple to write as mathematical definitions...
len :: Num p => [a] -> p
len [] = 0
len (x : xs) = 1 + len xs

-- ... as long as you know what the symbols mean.
-- [] denotes a list of some kind.
-- xs is commonly used as the name of a list.
-- x is the name of some value.
-- x:xs is simply a list with x at the front and xs behind it.
-- If you have a list you want to decompose, use the head and tail functions!
-- head(x:xs) = x (always returns a single element)
-- tail(x:xs) = xs (alwyas returns a list)

-- You can use recursion in Haskell too!
-- Before a function with parameters recurses, it evaluates any expressions passed as parameters.
-- This function computes the factorial of a number x.
fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact x = x * fact (x -1)

-- Notice how we begin with "fact 0 = 1"?
-- Haskell will check cases as it goes through the function definition.
-- Think the "if -> elif" structure from imperative languages.

-- When using the : operator, as long as the right side evaluates to a list, you're good to go!
-- Because of that, Haskell will evaluate the right side first.
-- That's how (x : y : xs) works!
select_evens :: [a] -> [a]
select_evens [] = []
select_evens [x] = []
select_evens (x : y : ys) = y : select_evens (ys)

select_odds [] = []
select_odds [x] = [x]
select_odds (x : y : ys) = x : select_odds (ys)

-- If you need to have some control flow, try the "if then else" structure!
-- When used in a recursive function, the function only recurses if the condition you give it is met
-- So this function checks if the current element of a list is equal to the key, m.
-- Otherwise it recurses on the next element of the list
-- source: https://en.wikibooks.org/wiki/Haskell/Control_structures
member m [] = False
member m (x : xs) =
  if m == x
    then True
    else member m xs

-- You can also use haskell's lazy boolean operators:
-- The boolean operator || means "or", however the right side of the operation isn't evaluated if the left side is true
-- -- Likewise, when evaluating &&, if the left side is false then the right side isn't evaluated
-- member2 [] y = False
-- member2 (x : xs) y = (x == y) || (member2 xs y)

-- The operator ++ is used to concatenate two lists
-- This is very useful so you don't have to use the : operator for everything
append_simple (x, y) = x ++ y

-- If you wanted to define your own concatenation operator, it would be something like this:
append [] [] = []
append xs [] = xs
append [] ys = ys
append (x : xs) (y : ys) = x : append xs (y : ys)

-- Here's how to reverse the order of a list:
revert [] = []
revert (x : xs) = append (revert (xs)) (x : [])

-- You can nest conditional statements too!

less_equal ([], []) = True
less_equal (x : xs, y : ys) =
  if len xs /= len ys
    then False
    else
      if x <= y
        then less_equal (xs, ys)
        else False

-- This function zips two lists together.
zipp ([], []) = []
zipp (x : xs, []) = x : xs
zipp ([], y : ys) = y : ys
zipp (x : xs, y : ys) = (x : y : []) ++ zipp (xs, ys)

-- As part of the larger merge_sort algorithm, the merge algorithm takes two sorted lists and combines them into another sorted list
-- It does this by checking the first element of each list and adding the smaller of the two to a new list.
-- It then moves on to the next element of the list that contained the smaller value, and recurses.
merge ([], []) = []
merge (x : xs, []) = x : xs
merge ([], y : ys) = y : ys
merge (x : xs, y : ys) =
  if (x <= y)
    then x : merge (xs, y : ys)
    else y : merge (x : xs, ys)

-- The actual merge_sort funcion simply splits an unsorted list in half, then recurses.
-- Once all the lists have a length of 1 or 0, the lists are merged in pairs until all the smaller lists are merged into one sorted list
merge_sort [] = []
merge_sort [x] = [x]
merge_sort xs = merge (merge_sort ys, merge_sort zs)
  where
    n = len (xs) `div` 2
    (ys, zs) = splitAt n xs

-- The "where" operator lets you define values to be used in an expression.
-- In this case, n is the index at which larger lists are split into smaller ones,
-- ys is the first half of the larger list, and zs is the second half.
-- source: http://www.cs.umd.edu/class/spring2019/cmsc388F/assignments/getting-to-know-haskell.html#:~:text=Merge%20Sort&text=The%20function%20mergeSort%20splits%20the,merges%20the%20two%20sorted%20halves.&text=Define%20the%20function%20splitHalf%20that,can%20use%20the%20function%20splitAt%20.)&text=Define%20the%20function%20merge%20that%20merges%20two%20sorted%20lists.
