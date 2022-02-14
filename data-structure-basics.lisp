;;;; Arrays
;;; Can have at least 7-dims, with 1023 elts in each
;; Make-array
(setf arr (make-array '(2 3) :initial-element nil))
;; Aref - retrieve elt of arr, zero index
(aref arr 0 0)
;; Replace - setf/aref
(setf (aref arr 0 0) 'b)
;; Array Literals - #na sytnax where n = arry dims 
; #2a((b nil nil) (nil nil nil))
(setf *print-array* t) ; prints arr's in literal form
;; Vectors - give make-array an integer arg 
(setf vec (make-array 4 :initial-element nil))
(vector "a" 'b 3)
;; Vector Acces - svref
(svref vec 0) ; NIL 

;;;; Strings and Characters
;;; "String" #\c
;;; code-char returns int assoc w/ char
;;; char< | char<= | char= | char>= | char> | char/= ---- Comparison Operators
;; Sort 
(sort "elbow" #'char)
;; Strings as Vectors - retrieval
(aref "abc" 1)
;; Strings as Chars - faster retrieval
(char "abc" 1)
;; Replace Elements - setf & char
(let ((str (copy-seq "Merlin")))
    (setf (char str 3) #\k)
    str)
;; String Comparison - equal & string-equal
(equal "fred" "fred") ; case-sensitive
(string-equal "fred" "Fred") ; not case-sensitive
;; String Building - format
(format nil "~A or ~A" "truth" "dare") ; most general way
;; String Concatenation - concatenate
(concatenate 'string "not " "to worry")

;;;; Sequences
;;; Type Sequence includes Lists and Vectors (& Strings)
;; General Element Retrieval - elt
(elt '(a b c) 1) ; Specific retrievel funcs are more optimized than elt
                 ; Only useful in generalized case-sensitive
;;; Lists only allow sequential access.
;;; Vectors allow random access.
;; Index Retrieval - position
(position #\a "fantasia") ; 1
(position #\a "fantasia" :start 3 :end 5) ; 4
(position #\a "fantasia" :from-end t) ; 7 - works backwards when :from-end == T
(position 'a '((c d) (a b)) :key #'car) ; 1 - :key = function applied to each elt
                                        ; asking for position of the first element 
                                        ; whose car is the symbol a
(position '(a b) '((a b) (c d))) ; nil - :test defaults to eql
(position '(a b) '((a b) (c d)) :test #'equal) ; 0 - use equal for lists
(position 3 '(1 0 7 5) :test #'<) ; 2 - :test can be func of any 2 args 
                                  ; testing to see position of first arg
                                  ; less than it
;; Sequence Decomposotion - subseq & position
(defun second-word (str) ; def a function that takes a string
    (let ((p1 (+ (position #\ str) 1))) ; p1 = position of first space + 1
        (subseq str p1 (position #\ str :start p1)))) ; :start = position to start at
                                                      ; find p1 in string from start index
                                                      ; to next whitespace
(second-word "Form follows function.") ; "follows"
;; Predicate Retrieval - position-if
(position-if #'oddp '(2 3 4 5)) ; 1 !!Doesn't take :test!!   
   



