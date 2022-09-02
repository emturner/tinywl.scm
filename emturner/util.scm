(define-module (emturner util)
  #:use-module (system foreign))

(define (repeat-aux acc value times)
  (if (<= times 0)
      acc
      (repeat-aux (cons value acc)
                  value
                  (- times 1))))

(define-public (repeat value times)
  "Constructs a list of length times by repeating value."
  (repeat-aux '() value times))

(define-public cstdbool int)

(define-public (cstdbool->bool cbool)
  (not (eq? 0 cbool)))

(define-public (bool->cstdbool bool)
  (if bool 0 1))
