theFibs' = 0 : 1 : (+) 0 1 : zipWith (+) (1 : zipWith (+) theFibs' (tail theFibs')) (zipWith (+) theFibs' (tail theFibs'))

sieve'' (0:ns) = sieve'' ns
sieve'' (n:ns) = n : sieve'' (mark (mark (mark (mark (mark ns 1 2) 1 3) 1 5) 1 7) 1 11) 
   where mark (y:ys) k m    | k == m    =  0 : (mark ys  1    m)
                            | otherwise =  y : (mark ys (k+1) m)
