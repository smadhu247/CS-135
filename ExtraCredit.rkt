#lang racket
;Sanjana Madhu
;I pledge my honor that I have abided by the Stevens Honor System

;Tree Method Extra Credit Assignment

;displays a window for user to input their proposition
(display "Please enter p if you would like to enter a proposition, or c to enter a conclusion")
(define var (read-line))
(cond ((equal? var "p") (display "Please enter your proposition (statements can include ors, nots, or ands): "))
      ((equal? var "c") (display "Please enter your conclusion: "))
      (else (display "Ok, Goodbye!") ))

;defines the proposition as a value
(define prop (read-line))

;negates a given conclusion
(define (negateConclusion prop)
  (cons "not" prop))

;splits proposition into a list of strings
(define (split lst)
  (let loop ((old '()) (new '()) (vals (string->list lst)))
    (cond ((null? vals)
           (reverse (cons (list->string (reverse new)) old)))
          ((char=? (car vals) #\space)
           (loop (cons (list->string (reverse new)) old)
                 '()
                 (cdr vals)))
          (else
           (loop old
                 (cons (car vals) new)
                 (cdr vals))))))


;defines the list of strings
(define list (split prop))

;;checks for an element in the list
(define (element? list val)
        (cond ((null? list) #f)
              ((equal? (car list) val) #t)
              (else (element? (cdr list) val))))

;replaces searchable values in list like and, not, or
(define (replace L first second)
   (cond ((null? L) L)
         ((list? L) (map(lambda (lst) (replace lst first second)) L))
         (else
          (if (equal? L second) first L))))

;checks for "not" and replaces with -
(define (notCheck list)
  (cond ((element? list "not" )  (replace list "-" "not") )
        (else #f)))

;checks for "or" and replaces with |
(define (orCheck list)
  (cond ((element? list "or" ) (replace list "|" "or"))
        (else #f)))

;checks for "and" and replaces with &
(define (andCheck list)
  (cond ((element? list "and")(replace list "&" "and"))
        (else #f)))

;checks whether or not DeMorgan's Law can be applied
(define (DemorganCheck list)
  (cond ((element? list "not") #t)
        ((and (element? list "if") (element? list "then")) #t)
        (else #f)))
