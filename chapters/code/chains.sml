datatype 'a chain = Link of 'a * ('a -> 'a chain)

fun ints n = Link (n+1, ints)

fun chain_item 1 (Link (element, _))= element
|   chain_item n (Link (element, next_link)) = 
        chain_item (n-1) (next_link element)

fun fibs n m = Link (n+m, fibs m)
