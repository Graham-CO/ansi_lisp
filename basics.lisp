;;;; Forms - these are all top-level (no indent)
1 ; atom
;;; Prefix Notation
(+ 2 3)     ; list - (operator arg arg)
(+ 2 3 4)   ;        (operator arg arg arg)
;; can take no arguments, or arbitrarily many
(+) 
(+ 2)
(+ 2 3)
(+ 2 3 4)
(+ 2 3 4 5)
;;; Nested Expressions
(/ (- 7 1) (- 4 2)) ; (op (op arg arg) (op arg arg))


;;;; Evaluation - args processed left-to-right, then vals passed to function
;;; Special Operators
(quote (+ 3 5)) ; do nothing, just return the arg as-is
'(+ 3 5)        ; this is the same thing - protects expression from evaluation


;;;; Data
;;; Symbols
'Artichoke  ; this is an unbound variable
            ; symbols don't evaluate to themselves so, quote it
;;; Lists
;; must be quoted, or will be evaluated function call
'(my 3 "Sons")  
'(the list (a b c) has 3 elements)  ; quote protects nested expressions
'(list 'my (+ 2 1) "Sons")
'(list '(+ 2 1) (+ 2 1))
;; empty lists
()
nil ; don't quote, already evaluates to self


;;;; List Operations
;;; cons - build Lists
(cons 'a '(b c d))
;;; car/cdr - extract list elements
(car '(a b c))  ; first element
(cdr '(a b c))  ; the rest
(car (cdr (cdr '(a b c d))))    ; these are 
(third '(a b c d))              ; equivalent

;;;; Logical Operations
;;; t - truth
(listp '(a b c)) ; returns true if arg is a list
                 ; p in function name stands for predicate (evaluated statement)
(listp 27)  ; return nil - two roles in lisp - empty & True/False
(null nil)  ; these both 
(not nil)   ; return t
;;; if statement - test expr, then expr, else expr 
(if (listp '(a b c))    ; 3
    (+ 1 2)
    (+ 5 6))
(if (listp 27)          ; 11
    (+ 1 2)
    (+ 5 6))
(if (listp 27)          ; nil
    (+ 2 3))            ; else statement defaults to nil
(if 27 1 2)             ; anything that isn't nil counts as true 
;;; and/or - take any # of args, only evaluate as many as needed to determine condition
(and t (+ 1 2)) ; if all args return true, return val. of last one
                ; or stops as soon as an arg that is true
;; These operators (and/or) are macros

;;;; Functions
;;; defun - definition
(defun our-third (x)    ; first arg. is func. name
  (car (cdr (cdr x))))  ; second arg. is list of accepted args. (variable vs parameter - x is both here)
                        ; rest is body of function
(our-third '(a b c d))
;; lists have to be quoted or will be treated as code
;; symbols have to be quoted or will be treated as a variable

;;;; Recursion
(defun our-member (obj lst)
    (if (null lst)
        nil
        (if (eql (car lst) obj) ; eql predicate tests if two args are identical
            lst
            (our-member obj (cdr lst)))))
(our-member 'b '(a b c))    ; (b c)
(our-member 'z '(a b c))    ; nil
;; think of recursion as a process - not a black box machine 

;;;; I/O
;;; format - takes two or more args.
;;;          first indicates where to print output
;;;          second is a string template
;;;          remaining args are objects whose printed reps. to be inserted into template
(format t "~A plus ~A equals ~A. ~%" 2 3 (+ 2 3))   ; 2 plus 3 equals 5
                                                    ; nil
#| 
first line is displayed by format
second line is val. returned by the call to format - usually don't see since 
    format isn't usually called from top-level
t indicates to send output to default place (top-level)
~A indicates fill pos, ~% indicates new line 
|#
;;; read - given no args, read from top-level
;; Prompt user for input, then return whatever entered
(defun askem (string)       
    (format T "~A" string)
    (read))
;; read is a complete lisp parser (how powerful? idk)
(askem "how u doin?") 

;;;; Variables
(let ((x 1) (y 2))  ; x & y valid in body of let  
    (+ x y))
;; revised askem
(defun ask-number () 
    (format t "Please enter a number. ")
    (let ((val (read)))     ; local var.
      (if (numberp val) 
            val
            (ask-number))))
(ask-number)
(defparameter *glob* 99)    ; global var.
(defconstant limit (+ *glob* 1)) ; global const.
(boundp '*glob*) ; check if global

;;;; Assignment
;;; setf - defaults to global scope
(setf *glob* 98)
(let ((n 10))
  (setf n 2) ; override scope
  n)
(setf x (list 'a 'b 'c))
;; in place insertion
(setf (car x) 'n) ; settable expressions
x

;;;; Functional Programming
;;; returning values from functions - not side-effects
(setf lst '(c a r a t))
(remove 'a lst)
lst ; origninal unchanged
(setf x (remove 'a x))  
;; want to avoid setf
;; want to rely only on values returned by functions

;;;; Iteration
;;; do - macro, fundamental iterator
#|
first arg - list of vars. (var init update)
second arg - list containing 1+ expr.
    1st: stop case
    rest: eval'd in order when iteration stops - last val returned
remaining - body of the loop
|#
(defun show-squares (start end) 
    (do ((i start (+ i 1))) 
        ((> i end) 'done) 
        (format t "~A ~A~%" i (* i i))))
(show-squares 2 5)
;; Recursive 
(defun show-squares (i end) 
    (if (> i end) 
        'done
        (progn ; takes n expr. eval in order & return last
            (format t "~A ~A~%" i (* i i))
            (show-squares (+i 1) end))))
;;; dolist - take arg of form (var expr), followed by body
;;;          Body eval'd w/ var bound to list elements returned by expr
(defun our-length (lst) 
    (let ((len 0))
      (dolist (obj lst) 
        (setf len (+ len 1)))
        len))
(our-length '(a b c))
;; Recursive
(defun our-length (lst) 
    (if (null lst)
        0
        (+ (our-length (cdr lst)) 1)))

;;;; Functions as objects
(function +) ; given func. name, return objects
             ; special operator
;;; sharp-quote #' is like ' for functions
(apply #'+ 1 2 '(3 4 5))
(funcall #'+ 1 2 3) ; doesn't need args as list
;;; lambda expr. - list of parametrs, body of 0+ expr
(lambda (x y) 
    (+ x y))
((lambda (x) (+ x 100)) 1)  ; calling lambda
(funcall #'(lambda (x) (+ x 100))
    1)

;;;; Types
;;; manifest typing - values have types, not the var.
;;;                   type hierarchy
(typep 27 'integer) ; type-check


;;;;
;;;;
;;;;
;;;; Practice
;;; 1
;; Describe each
(+ (- 5 1) (+ 3 7))
; (5-1) + (3+7)
(list 1 (+ 2 3))
; create a list of (1 5)
(if (listp 1) (+ 1 2) (+ 3 4))
; since predicate ret false, do else statement (+ 3 4)
(list (and (listp 3) t) (+ 1 2))
; create list (nil 3)
;;; 2
;; Give 3 cons that return (a b c)
(cons 'a '(b c))
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cdr '(a b c)))
;;; 3
;; Using car/cdr, def func to ret. 4th element
(defun my-fourth (x) 
    (car (cdr (cdr (cdr x)))))
(my-fourth '(a b c d))
;;; 4 
;; Def func. that takes 2 args & ret. greater
(defun greater-than (x y) 
    (if (> x y) x y))
(greater-than 10 2)
;;; 5
;; what do these do?
;; a
(defun enigma (x) ; def function called enigma, takes var. x
    (and (not (null x)) ; if x isn't null
         (or (null (car x)) ; and the first val. of x is not null
            (enigma (cdr x))))) ; recursively call enigma with rest of x
(enigma '(1 2 3 4))
; it just runs through a list, doing nothing until the end nil
;; b
(defun mystery (x y)  ; def func mystery takes x and y
    (if (null y) ; if y is null
        nil      ; return nil
        (if (eql (car y) x) ; elseif the beginning of y and x are equal
            0 ; return 0
            (let ((z (mystery x (cdr y)))) ; else define z as a call to mystery with the cdr of y
              (and z (+ z 1))))))
(mystery 1 '(1 2 1 1 3)) 
; recurse through list, checking for x in y
; if found, return 0
; else, eventually z will be set to nil, which makes the last line
; return a value of nil, I.e, check for x in y
;;; 6
;; See book for this one 
(car (car (cdr '(a (b c) d))))
(or 13 (/ 1 0))
(apply #'list 1 nil)
;;; 7
;; Def func that takes list 
(defun exists-a-list (lst)
    (if (null lst) 
        nil
        (if (listp (car lst)) 
            t
            (exists-a-list (cdr lst)))))
(exists-a-list '(a (b c) d))
;;; 8
;;; 9













