(define matrix1 `((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(define matrix2 '((0 1 2)
                  (3 4 5)
                  (6 7 8)))

(define (get-nth-element matrix n)
  (if (= n 0) (car matrix) (get-nth-element (cdr matrix) (- n 1)))
)

#|Matrix operatiors|#

(define (matrix-col matrix c)
  (map (lambda (x) (get-nth-element x c)) matrix))


#|
1. Дефинирайте функция (sum matrix), която намира сумата на всички елементи в матрица
|#
(define (reduce op accumulator list)
  (if (equal? list `()) accumulator (reduce op (op accumulator (car list)) (cdr list)))
)

(define (sum-matrix matrix)
  (reduce + 0 (map (lambda (x) (reduce + 0 x)) matrix)))

#|
2. Дефинирайте функция (diagonal matrix), която връщa диагонала на дадена матрица
|#
(define (get-diag matrix)
  (define (inner-diag matrix n accum)
    (if(equal? matrix `()) accum (inner-diag (cdr matrix) (+ n 1) (append accum (list (get-nth-element (car matrix) n)))))
    )
  (inner-diag matrix 0 `())
  )

#|
3. Дефинирайте функция (min matrix), която намира най-малкия елемент в дадена матрица.
|#
(define (list-min list)
  (define (list-min-inner list minVal)
    (if (equal? list `()) minVal (list-min-inner (cdr list) (min minVal (car list))))
    )
  (list-min-inner list (car list))
  )

(define (_min matrix)
  (list-min (map (lambda (x) (list-min x)) matrix)))

#|
4. Дефинирайте функция (transpose matrix), която транспонира матрица.
|#
(define (transpose matrix)
  (define len (length (car matrix)))
  (define (inner-transpose matrix col accum)
    (if (= col len) accum (inner-transpose matrix (+ col 1) (append accum (list(matrix-col matrix col)))))
    )
  (inner-transpose matrix 0 `())
  )


#|
5. (Георги)  Дефинирайте функция (set matrix x i j), която връща същата матрица,
            но на позиция (i, j) се намира елемента x.
|#
(define (from-to listt a b)
  (define (inner-from-to listt a b result)
    (if (> a 0) (inner-from-to (cdr listt) (- a 1) (- b 1) result) 
    (if (= a b) result (inner-from-to (cdr listt) a (- b 1) (append result (list (car listt)))))
    )
  )
  (inner-from-to listt a b `())
)

(define (set matrix n i j)
  (define rc (length matrix))
  (define cc (length (car matrix)))
  (define (inner-set matrix n i j k accum)
    (cond
      ((= k rc) accum)
      ((= i k) (inner-set matrix n i j (+ k 1) (append accum (list (append (append (from-to (get-nth-element matrix i)
                                                                                  0 j)
                                                                         (list n))
                                                                 (from-to(get-nth-element matrix i) (+ j 1) cc))))))
      (else   (inner-set matrix n i j (+ k 1)  (append accum (list (get-nth-element matrix k)))))
    )
  )
  (inner-set matrix n i j 0 `())
)


#|
6. Дефинирайте функции (skip-row matrix index) и (skip-column matrix index),
  които връщат нови матрица,
  с премахнат съответно ред или колона index от matrix.
|#
(define (skip-row matrix row)
  (define (skip-row-inner matrix row i accum)
    (if (equal? matrix `()) accum
        (if (= i row) (skip-row-inner (cdr matrix) row (+ i 1) accum)
                      (skip-row-inner (cdr matrix) row (+ i 1) (append accum (list (car matrix))))
        )
    )
  )
  (skip-row-inner matrix row 0 `())
)

(define (skip-col matrix c)
  (define len (length (car matrix)))
  (define (inner-skip-col matrix col j accum)
    (if (= j len) accum
        (if (= j col) (inner-skip-col matrix col (+ j 1) accum)
                    (inner-skip-col matrix col (+ j 1) (append accum (list(matrix-col matrix j)))))
        )
    )
  (inner-skip-col matrix c 0 `())
)


#|
7. Дефинирайте функция (sum matrix1 matrix2), която събира две матрици
|#

(define (accum-lists op l1 l2)
  (define (inner-accum-lists + l1 l2 result)
    (if (or (equal? l1 `()) (equal? l2 `())) result
                                   (inner-accum-lists + (cdr l1) (cdr l2) (append result (list (op (car l1) (car l2))))))
    )
  (inner-accum-lists + l1 l2 `())
)

(define (sum-matrix m1 m2)
  (define (inner-sum-matrix m1 m2 accumulator)
    (if (or (equal? m1 `()) (equal? m2 `()))
        accumulator
        (inner-sum-matrix (cdr m1) (cdr m2) (append accumulator (list (accum-lists + (car m1) (car m2)))))
    )
  )
  (inner-sum-matrix m1 m2 `())
)


#|
8. Дефинирайте функция (product matrix1 matrix2), която умножава две матрици.
|#
(define (product-matrix m1 m2)
  (define transposed-m2 (transpose m2))
  (define (inner-pm m1 tm2 accumulator)
    (if (or (equal? m1 `()) (equal? tm2 `()))
        accumulator
        (inner-pm (cdr m1) tm2 (append accumulator
                                            (list
                                             (map (lambda(x) (reduce + 0 (accum-lists * (car m1) x)))
                                                  tm2)
                                             )
                                    )
        )
    )
  )
  (inner-pm m1 transposed-m2 `())
)



#|
9. (Анди) Дефинирайте функция (triangular? matrix),
         която проверява дали дадена матрица е горно-триъгълна
         (всички елементи под главния диагонал са нули).
|#
(define matrix3 `((1 2 3 4)
                  (0 5 6 7)
                  (0 0 8 9)
                  (0 0 0 1)))

(define (triangular? m1)
  (define (row-loop i m1)
    (define (col-loop cols j m1)
      (if (= j cols)
          #t
          (if (= (car m1) 0)
              (col-loop cols (+ j 1) (cdr m1))
              #f
          )
      )
    )
    (if (equal? m1 `())
        #t
        (if (col-loop i 0 (car m1)) (row-loop (+ i 1) (cdr m1))
            #f
        )
    )
 )
 (row-loop 0 m1)
)
















    