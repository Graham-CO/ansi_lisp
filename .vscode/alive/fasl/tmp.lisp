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


