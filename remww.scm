(define remww
  (lambda (input)
    (reverse! (remww_helper (reverse! input) '()))
    ))

(define remww_helper
  (lambda (lst pairs)
;    (display (caddar lst)) (newline) (newline)
    (cond ((null? lst) lst)
	  ((null? (caddar lst)) (remww_helper (cdr lst) pairs))
	  ((not_appears_right? (caddar lst) pairs) (cons (car lst) (remww_helper (cdr lst) (cons (cons (cadar lst) (caddar lst)) (remove_left (caddar lst) pairs)))))
	  ((appears_left? (caddar lst) pairs) (cons (car lst) (remww_helper (cdr lst) (cons (cons (cadar lst) (caddar lst)) (remove_left (caddar lst) pairs)))))
	  (else (remww_helper (cdr lst) pairs)))
    ))

(define not_appears_right?
  (lambda (lst pairs)
					;    (display lst) (newline) (display pairs) (newline) (newline)
    (ormap (lambda (x) (andmap (lambda (y) (not (member x (cdr y)))) pairs)) lst)
    ))

(define appears_left?
  (lambda (lst pairs)
    (ormap (lambda (x) (ormap (lambda (y) (member y (car x))) lst)) pairs)
    ))

(define remove_left
  (lambda (lst pairs)
    (map (lambda (x) (cons (filter (lambda (y) (not (member y lst))) (car x)) (cdr x))) pairs)
    ))
	  
