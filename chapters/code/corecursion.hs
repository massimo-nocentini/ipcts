
import Data.Array
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

------------------------------------------------------------------------
-- from `Building recursive data structures in Haskell` by Duncan Coutts
------------------------------------------------------------------------

repeat' :: a -> [a]
repeat' x = xs where xs = x : xs

-- doubly linked lists
data DoublyLinked a = Nil | Node a (DoublyLinked a) (DoublyLinked a)

instance Show a => Show (DoublyLinked a) where
    show Nil = ""
    show (Node a _ _) = show a

next_node (Node _ _ n) = n
prev_node (Node _ p _) = p

-- write a function that promotes a list to a doubly linked one:
to_doubly_linked :: [a] -> DoublyLinked a
to_doubly_linked l = doubly_linked l Nil

doubly_linked [] _ = Nil
--doubly_linked (x:xs) prev = Node x prev (doubly_linked xs <>)
-- <> denotes the node we are actually building, namely:
-- Node x prev (doubly_linked xs <>) itself! ...so name it!
doubly_linked (x:xs) prev = node
    where node = Node x prev (doubly_linked xs node)

-- graphs with single out connection
data SingleOutGraph a = SONode a (SingleOutGraph a)

make_sograph :: [(a, Int)] -> Int -> SingleOutGraph a
--make_sograph table root = ( ! root) . listArray (0, length table - 1) $ map (\(elem, i) -> SONode elem (<> ! i) table
-- <> denotes a table that holds other SONodes and, therefore, holds the node we're 
-- currently building with the lambda expression, so name such table:
make_sograph table root = table' ! root 
    where table' = listArray (0, length table - 1) $ map (\(elem, i) -> SONode elem (table' ! i)) table

-- general graphs
data GeneralGraph a = GNode a [GeneralGraph a]

make_ggraph :: [(a, [Int])] -> Int -> GeneralGraph a
make_ggraph table root = table' ! root 
    where table' = listArray (0, length table - 1) $ map (\(elem, is) -> GNode elem (map (table' !) is)) table

-- streams
data Stream a = Cons {hd :: a, tl :: Stream a}

infixr 5 <<=
(<<=) :: a -> Stream a -> Stream a
a <<= s = Cons a s

turn :: Int -> [Int]
turn 0 = []
turn n = turn (n-1) ++ [n-1] ++ turn (n-1)
