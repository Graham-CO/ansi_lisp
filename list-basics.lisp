;;;; Conses
;;; Constructs x onto y - x == car, y == cdr
;;; Lists are conses of conses

;; Nested List
(setf z (list 'a (list 'b 'c) 'd))
;; Flat List
(setf y (list 'a 'b 'c))

;;;; Equality
;; (cons A B) always has a unique memory address 
(eql (cons 'a nil) (cons 'a nil)) ; NIL
(setf x (cons 'a nil))
(eql x x) ; T - checks if same objects
(equal x (cons 'a nil)) ; T - checks if same elements

;;;; List Building
(setf x '(a b c)        ; y will have same elements as x
      y (copy-list x))  ; but in new conses
;; Concatenation
(append '(a b) '(c d) '(e))

;;;; Access
;; Zero-indexed
(nth 0 '(a b c)) ; A
(nthcdr 2 '(a b c)) ; C
(last '(a b c)) ; C 
;;; functions first - tenth (not zero indexed)

;;;; Mapping
;; mapcar - takes a function and 1+ lists, applies function to elements of lists
(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))                   ; (11 12 13)

(mapcar #'list
        '(a b c)
        '(1 2 3 4))                 ; ((A 1) (B 2) (C 3))

;; maplist - does the same, but acts on cdr's of lists

;;;; Trees 
;;; Lisp conses can also be thought of as binary trees - car == left, cdr == right 
(substitute 'y 'x '(and (integerp x) (zerop (mod x 2)))) ; wont replace anything since the elements arent actually x - just contain x 
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))      ; will replace, since it works on tree nodes

;;;; Sets 
;; Member
(member 'b '(a b c)) ; compare using eql
(member '(a) '((a) (z)) :test #'equal) ; kwargs start with :
(member 'a '((a b) (c d)) :key #'car)  ; apply func to each elt before comparison
;; Member-if
(member-if #'oddp '(2 3 4)) ; elt checking with predicate
;; Adjoin
(adjoin 'b '(a b c)) ; (A B C) - cons if not already member
(adjoin 'z '(a b c)) ; (Z A B C)
;;; These don't necessaryily preserve order
;; Union
(union '(a b c) '(c b s)) ; (A B C S)
;; Intersection
(intersection '(a b c) '(b b c)) ; (B C)
;; Set-difference
(set-difference '(a b c d e) '(b e)) ; (A C D)


;;;; Sequences
(length '(a b c)) ; 3
;; Subseq - copy
(subseq '(a b c d) 1 2) ; (B) - 1 == first to be included -- 2 == first to not be included
(subseq '(a b c d) 1) ; (B C D) - go from first elt included to end 
;; Reverse
(reverse '(a b c)) ; (C B A)
;; Sort - destructive: modifies the sequence passed to it directly 
(sort '(0 2 1 3 8) #'>) ; (8 3 2 1 0)
;; Every & Some
(every #'oddp '(1 3 5)) ; T
(some #'evenp '(1 2 3)) ; T 
(every #'> '(1 3 5) '(0 2 4)) ; if sequences of diff length, shortest determines # of test

;;;; Stacks
;; Push & Pop 
(setf x '(b)) ; (B)
(push 'a x)   ; (A B) - add to front of stack
(pop x)       ; A - rmv and ret first elt
;; Pushnew
(let ((x '(a b))) ; c added, but a not b/c already member
  (pushnew 'c x)
  (pushnew 'a x)) ; (C A B) 

;;;; Dotted Lists
;;; Proper List - only nil or a cons whose cdr is a proper list 
;; Two part data structure (doesn't point anywhere)
(setf pair (cons 'a 'b)) ; (A . B)
;; Dot Notation - not used
'(a . (b . (c . nil)))   ; (A B C)

;;;; Assoc-lists
;;; List of conses - works like a named tuple or a dictionary
(setf trans '((+ . "add") (- . "subtract")))
(assoc '+ trans) ; (+ . "add") - retrieves pair 
(assoc '* trans) ; NIL 
;;; Assoc takes kwargs :test & :key
;;; Lisp defines assoc-if

