# Filter

filterサンプル

```scheme
(define (filter f list)
  (if (null? list)
      ()
      (if (f (car list))
          (cons (car list) (filter f (cdr list)))
          (filter f (cdr list)))))
```
