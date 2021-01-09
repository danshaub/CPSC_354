-- numbers
data NN = O | S NN
  deriving (Eq, Show)

nn_to_int :: Num p => NN -> p
nn_to_int O = 0
nn_to_int (S n) = 1 + nn_to_int (n)

-- addition
addNN :: NN -> NN -> NN
addNN O n = n
addNN (S n) m = S (addNN n m)

-- add (S O) (S (S O))

-- multiplication

multNN :: NN -> NN -> NN
multNN n O = O
multNN n (S m) = addNN n (multNN n m)

-- subtraction

subNN :: NN -> NN -> NN
subNN n O = n
subNN O n = O
subNN (S n) (S m) = subNN n m

-- modulo division

modNN :: NN -> NN -> NN
modNN n O = error "Divide by zero"
modNN O m = O
modNN n m =
  --If the two numbers are equal, return O
  if n == m
    then O
    else --Otherwise subtract m from n until n < m
    --Then return n

      if (subNN n m) == O
        then n
        else modNN (subNN n m) m

-- division

divNN :: NN -> NN -> NN
divNN n O = error "Divide by zero"
divNN O m = O
divNN (S n) m =
  -- Check if (S n) is evenly divisible by m
  if (modNN (S n) m) == O
    then -- If it is, append an S onto the result and recurse
      S (divNN n m)
    else --otherwise, recurse without appending
      divNN n m

-- exponentiation
powNN :: NN -> NN -> NN
powNN O O = error "not in domain"
powNN O n = O
powNN n O = (S O)
powNN n (S m) = multNN n (powNN n m)

data Exp = Num Int | Times Exp Exp | Add Exp Exp | Divide Exp Exp | Subtract Exp Exp | Modulo Exp Exp | Power Exp Exp
  deriving (Eq, Show)

eval :: Exp -> NN
eval (Num 0) = O
eval (Num n) = S (eval (Num (n - 1)))
eval (Times n m) = multNN (eval n) (eval m)
eval (Add n m) = addNN (eval n) (eval m)
eval (Divide n m) = divNN (eval n) (eval m)
eval (Subtract n m) = subNN (eval n) (eval m)
eval (Modulo n m) = modNN (eval n) (eval m)
eval (Power n m) = powNN (eval n) (eval m)
