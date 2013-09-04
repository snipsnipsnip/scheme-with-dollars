scheme-with-dollars, or $cheme
===================

(watch your step before reading below)

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

could be written like this:

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

or even like this.

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

I love haskell.
