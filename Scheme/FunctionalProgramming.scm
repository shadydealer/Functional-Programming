(define (_even? number)
  (= (remainder number 2) 0)
)


(define (triangle? a b c)
  (and
   (> (+ a b) c)
   (> (+ a c) b)
   (> (+ b c) a)
   )
)

(define (slow-factorial number)
  (if (= number 0) 1 (* number (slow-factorial (- number 1))))
)

(define (fast-factorial number)
  (define (inner-fact number acum)
    (if (= number 0) acum (inner-fact (- number 1) (* acum number))))  
  (inner-fact number 1)
)

(define (power base exp)
  (define (inner-power base exp acum)
    (if(= exp 0) acum (helper base (- exp 1) (* base acum)))
  )
  (inner-power base exp 1)
)

(define (homebrew-gcd a b)
  (if (= a b) a (if (> a b) (homebrew-gcd (- a b) b) (homebrew-gcd a (- b a))))
)

(define (slow-sum start end)
  (define (inner-slow-sum start end accum)
    (if (> start end) accum (inner-sum (+ start 1) end (+ accum start)))
  )
  (inner-slow-sum start end 0)
)

(define (fast-sum start end)
  (-(/ (* end (+ end 1)) 2) (/ (* start (+ start 1)) 2))
)

(define (slow-fibonacci n)
    (if (< n 2) 1 (+(slow-fibonacci (- n 1)) (slow-fibonacci (- n 2))))
)

(define (prime? n)
  (define (inner-prime n i)
    (cond
      ((=(remainder n i) 0) #f)
      ((<= (sqrt n) i) #t)
      (else (inner-prime n (+ i 2)))
    )
  )
  (if (and (not(= n 2)) (=(remainder n 2) 0)) #f (inner-prime n 3))
)

(define (decimal-to-binary n)
  (if (= n 0) 0 (+(remainder n 2) (* 10 (recursive-dtb(quotient n 2)))))
)

(define (homebrew-reverse n)
  (define (inner-hr n result)
    (if (= n 0) result
    (inner-hr (quotient n 10) (+ (remainder n 10) (* result 10))))
  )
  (inner-hr n 0)
)

(define (accumulate op nil from to step term)
  (define (inner-accumulate op nil from to step term accumulator)
    (if(> from to) accumulator
       (inner-accumulate op nil (+ from step) to step term (op accumulator (term from)))))
  (inner-accumulate op nil from to step term nil))

