scheme-with-dollars, or $cheme
===================

(Be careful not to fall forward)

This:

```scheme
(define Y
  (lambda (f)
    ((lambda (x)
        (f (lambda (v)
              ((x x)
                v))))
      (lambda (x)
        (f (lambda (v)
              ((x x)
                v)))))))
```

becomes like this:

```scheme
(define Y
  (lambda (f
    ((lambda (x
        (f (lambda (v
              ((x x
                v 
      (lambda (x
        (f (lambda (v
              ((x x
                v
```

or like this.

```scheme
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
```
