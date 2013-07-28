(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (unique-three-pairs n)
  (accumulate
   append
   (list (list))
   (accumulate
    append
    (list (list))
    (map (lambda (x)
           (map (lambda (y)
                  (map (lambda (z)
                         (list x y z))
                       (enumerate-interval (+ y 1) n)))
                (enumerate-interval (+ x 1) n)))
         (enumerate-interval 1 n)))))

 
(define (equal-three-pairs n s)
  (filter (lambda (x)
            (= s (accumulate + 0 x)))
          (unique-three-pairs 1 n)))
