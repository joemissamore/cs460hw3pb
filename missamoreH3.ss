;
; Joe Missamore
; CS 460 
; 10/22/2018
;
; Homework 3 part b

(define (reciprocal n)
 (if (and (number? n) (not (= n 0)))
 (/ 1 n)
'invalid_parameter
 )
)


 (define (square n)
(if (number? n)
 (* n n)
 'invalid_parameter
)
)

; returns a copy of a list
(define (list_copy ls)
 (if (list? ls)
 ls
 'list_copy1_requires_a_list_argument
 )
)

; returns a copy of every odd placed element
(define (odd_copy ls)
 (if (list? ls)
 (if (null? ls)
 '()
 (if (null? (cdr ls))
 ls
 ; grabs the first element and 
 ; cuts off the of the first and second element
(cons (car ls) (odd_copy (cddr ls)))
 )
 )
 'odd_copy_requires_a_list_argument
 )
)

; returns a copy of every even placed element
(define (even_copy ls)
 (if (list? ls)
 (if (null? ls)
 ls
 (if (null? (cdr ls))
 '()
 ; cuts the first element out of the list and takes the car 
 ; of the returning list followed by cutting the first 
 ; and second positions out recursively.
  (cons (car (cdr ls)) (even_copy (cddr ls)))
 )
 )
 'even_copy_requires_a_list_argument
 )
)

; returns a sum of a list of ints
(define (list_sum mylist)
    (if (list? mylist)
    (if (null? mylist)
        0 ; if the list is null return 0
        (+ (car mylist) (list_sum (cdr mylist))) ; recursively add all the sums in the list
    )
    )
)

(define (last mylist)
    (if (list? mylist)
    (if (null? mylist)
        '()
    (if (null? (cdr mylist))
        (car mylist) ; if the list is null after taking the cdr its the last element
        (last (cdr mylist)) ; recursively shorten the list
    )
    )
    'last_requires_a_list_argument
    )
)

(define (insert_last myvalue mylist) 
    (if (list? mylist)
    (if (null? mylist)
        (list myvalue) ; the value to be inserted is returned with the list is ()
        (cons (car mylist) (insert_last myvalue (cdr mylist))) ; insert all the elements of the list recursively
    )
    'insert_last_requires_a_list_argument
    )
)

(define (remove_first mylist) 
    (if (list? mylist)
    (if (null? mylist)
        '()
        (cdr mylist) ; remove the first element
    )
    )
)

(define (remove_last mylist) 
    (if (list? mylist)
    (if (null? mylist)
        '()
    (if (null? (cdr mylist)) ; if true, this is the last element so we will remove it by returning an empty list
        '()
        (cons (car mylist) (remove_last (cdr mylist)))
    )
    )
    )
)

(define (list_reverse mylist) 
    (if (list? mylist)
    (if (null? mylist)
        '()
        (cons (last mylist) (list_reverse(remove_last mylist))) ; utilizing functions made
    )
    )
)


(define (square_root n)
    (round (square_root_helper n n 1 0.000001))
)

; helper function for square root
(define (square_root_helper n x y e)
    (if ( > (- x y) e)
        (square_root_helper n (/ (+ x y) 2) (/ n (/ (+ x y) 2)) e)
        x ; once (x - y) < e we will return x
    )
)

; round to the second place
(define (round n)
    (/ (floor (* 100000 n)) 100000)
)