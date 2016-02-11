
-- From: http://blog.sigfpe.com/2007/07/data-and-codata.html

------------------------------------------------------------------------

data EnhancedNat = Zero | Succ EnhancedNat | Prev EnhancedNat deriving Show

one = Succ Zero

-- This isn't well behaved at all
sum' a = sum' (1:a) - 1

sum'' a = Prev (sum'' (one:a))

minus m Zero = m
--minus Zero _ = undefined
minus m (Prev n) = Succ (minus m n)
minus m (Succ n) = Prev (minus m n)
-- a formal rewriting could yield the following,
-- but introduces second-order recursion:
--minus m (Succ n) = minus (minus m n) one

-- maybe the following rules are really wrong!
--minus (Succ m) n = Succ (minus m n)
--minus (Prev m) n = Prev (minus m n)

sum''' a = minus (sum''' (one:a)) one

matched_sum (Prev (Prev (Prev a))) = a

-- Should we simply rule out infinite datastructures? 
-- That seems a bit drastic. The example that convinced 
-- me to look into Haskell was:
fib = 0 : 1 : zipWith (+) fib (tail fib)

{-
 Like `sum`, `sumSoFar` defined belowe fails to terminate for an infinite input. 
 But unlike sum, it's possible to make sense of it. 
 If the inputs were 0 and the infinite list [1,1,1,1,...] 
 then the result would be [0,1,2,3,...]
 -}
sumSoFar x [] = [x]
sumSoFar x (y:ys) = x : sumSoFar (x+y) ys

