
$ define Y
  $ lambda $ f
    $ $ lambda $ x
        $ f $ lambda $ v
              $ $ x x
                v 
      $ lambda $ x
        $ f $ lambda $ v
              $ $ x x
                v 

$ define filter
  $ Y
    $ lambda $ filter
      $ lambda $ f xs
        $ if $ null? xs
          xs
          $ if $ f $ car xs
            $ cons $ car xs
                   $ filter f $ cdr xs
            $ filter f $ cdr xs

$ define $ list-equal? eq? xs ys
  $ or $ and $ null? xs
             $ null? ys
       $ and $ not $ or $ null? xs
                        $ null? ys
             $ eq? $ car xs
                   $ car ys
             $ list-equal? eq? $ cdr xs
                               $ cdr ys

$ define $ test1
  $ let $ $ even?
            $ lambda $ n
              $ = 0 $ mod n 2
    $ list-equal? =
      '$ 2 4 6 8 10
      $ filter even? '$ 1 2 3 4 5 6 7 8 9 10

$ define $ test2
  $ define $ zero? n
    $ = n 0

  $ letrec $ $ even? 
               $ lambda $ n
                 $ if $ zero? n
                   #t
                   $ odd? $ - n 1
             $ odd?
               $ lambda $ n
                 $ if $ zero? n
                   #f
                   $ even? $ - n 1
    $ even? 88

$ print $ test1
$ print $ test2
