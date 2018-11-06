#|
1.  Да се напише функция middle-digit, която намира средната цифра от записа на подадено естествено число n.
   Ако n е с четен брой цифри, функцията връща -1.
|#
(define (digit-count number)
  (define (inner-digit-count number accum)
    (if (< number 10)
        accum
        (inner-digit-count (quotient number 10) (+ accum 1))
    )
  )
  (inner-digit-count number 1)
)
      
(define (middle-digit number)
  (define digits (digit-count number))
  (define (inner-middle-digit number n)
    (if (= n 0)
        (remainder number 10)
        (inner-middle-digit (quotient number 10) (- n 1))
    )
  )
  (if (= (remainder digits 2) 0)
      -1
      (inner-middle-digit number (quotient digits 2))
  )
)

#|
2. Нека е даден списък l от числа и двуместна операция над числа ⊕.
  Функцията f наричаме “ендоморфизъм над l”, ако f трансформира l в себе си, запазвайки операцията ⊕,
  т.е. ∀x∈l f(x)∈l и ∀x,y∈l f(x) ⊕ f(y) = f(x ⊕ y).
  Да се реализира функция is-em?, която проверява дали f е ендоморфизъм.
|#


(define some-list `(0 1 4 6))

(define (surjective? list func)
  (define (inner-surjective full-list temp-list func)
    (if (equal? temp-list `())
        #t
        (if (member (func (car temp-list)) full-list)
            (inner-surjective full-list (cdr temp-list) func)
            #f
        )
    )
  )
  (inner-surjective list list func)
)

(define (keeps-operation? list op func)
  (define (inner-keeps-operation first-list second-list op func)
    (if (equal? first-list `()) #t
        (if (equal? second-list `())
            (inner-keeps-operation (cdr first-list) list op func)
            (if (= (op (func (car first-list)) (func(car second-list))) (func (op (car first-list) (car second-list))))
                (inner-keeps-operation first-list (cdr second-list) op func)
                #f
            )
        )
    )
  )
  (inner-keeps-operation list list op func)
)

(define (is-em? list op func)
  (and (surjective? list func) (keeps-operation? list op func))
)

#|
3. Да се напише функция (meet-twice? f g a b), която проверява дали в целочисления интервал [a, b]
  съществуват две различни цели числа x и y такива, че f(x) = g(x) и f(y) = g(y).
|#


(define (meet-twice? f g a b)
  (define (inner-meet-twice? f g a b counter)
    (if (= counter 2)
        #t
        (if (>= a b)
            #f
            (if (= (f a) (g a))
                (inner-meet-twice? f g (+ a 1) b (+ counter 1))
                (inner-meet-twice? f g (+ a 1) b counter)
                )
            )
        )
    )
  (inner-meet-twice? f g a b 0)
)

#|
4. Казваме, че списъкът x = (x1 x2 … x2n) от цели числа се получава от прочитането (look-and-say) на списъка y,
  ако y се състои от последователно срещане на x1 пъти x2, последвано от x3 пъти x4, и така нататък до x2n-1 пъти x2n.
  Да се дефинира функция next-look-and-say, която по даден списък y намира списъка x, получен от прочитането y.
|#

(define (look-and-say rhs-list)
  (define (inner-look-and-say rhs-list previous-item counter accum)
    (if (equal? rhs-list `())
        (if (>= counter 1) (append accum (list counter previous-item)) accum)
        (if (equal? (car rhs-list) previous-item)
            (inner-look-and-say (cdr rhs-list) (car rhs-list) (+ counter 1) accum)
            (inner-look-and-say (cdr rhs-list) (car rhs-list) (- counter (- counter 1)) (append accum (list counter previous-item)))
        )
    )
  )
  (if (equal? rhs-list `())
      `()
      (inner-look-and-say rhs-list (car rhs-list) 0 `())
      )
)

#|
5. Да се напише функция (longest-descending­ l), която намира низходящо сортиран подсписък на списъка от числа l с максимална дължина.
  Ако съществуват няколко такива подсписъка, функцията да върне първия отляво надясно.
  Упътване: Реализирайте помощна функция, която намира най-дългия низходящо сортиран префикс на даден списък.
|#
(define (longest-descending­ l)
  (define (inner-longest-descending l prev-item longest-sublist curr-desc-sublist)
    (if(null? l)
       (if (>= (length longest-sublist) (length curr-desc-sublist)) longest-sublist curr-desc-sublist)
       (if (<= (car l) prev-item)
           (inner-longest-descending (cdr l) (car l) longest-sublist (append curr-desc-sublist (list (car l))))
           (if (>= (length longest-sublist) (length curr-desc-sublist))
               (inner-longest-descending (cdr l) (car l) longest-sublist (list (car l)))
               (inner-longest-descending (cdr l) (car l) curr-desc-sublist (list (car l)))
               )
           )
       )
    )
  (inner-longest-descending l (car l) `() `())
)




#|
Some other functions
|#

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l)
      nv
      (op (foldl op nv (cdr l)) (car l)
          )
      )
  )

(define (filter p? l)
  (foldr (lambda (h t) (if (p? h) (cons h t) t)) '() l))
