;;;; Run Length Encoding - Naive 

;;; Function to compress a list 
;; accepts a list as input and outputs a run-length-encoded list representation
(defun compress (x)
  (if (consp x)                         ; if the list is not empty
      (compr (car x) 1 (cdr x))         ; call helper function 
      x))                               ; else return the empty list

;;; Helper function - recursive call to traverse list and check for same symbols
;; accepts: the element we last saw, the no. times it's been seen, part of list yet to be checked
(defun compr (elt n lst)
  (if (null lst)                                    ; if at the end of the list
      (list (n-elts elt n))                         ; call helper function
      (let ((next (car lst)))                       ; else go to the next elemt
        (if (eql next elt)                          ; if the next element is same as previous
            (compr elt (+ n 1) (cdr lst))           ; recursive call, with n incremented
            (cons (n-elts elt n)                    ; else append the compressed segement to a
                  (compr next 1 (cdr lst)))))))     ; recursive call with rest of the list

;;; Helper function - creates the compressed list segments 
;; accepts: the element being counted, the number of times it was counted
(defun n-elts (elt n)
  (if (> n 1)           ; if number of occurences is greater than 0
      (list n elt)      ; replace them with a list
      elt))             ; otherwise leave them be 

;;; Function to uncompress a list - expands the list from start->end then rebuilds from end->start
(defun uncompress (x)
  (if (null lst)                            ; empty list check
      nil                                   
      (let ((elt (car lst))                 ; recursively expand the list
            (rest (uncompress (cdr lst))))  
        (if (consp elt)                     ; as it backs out - check if you're at nil 
            (append (apply #'list-of elt)   ; if not, expand that segment and append to the end
                    rest)                   ; so that (latest in, previous, nil)
            (cons elt rest)))))             

;;; Helper function - expand compressed segments (use make-list instead)
(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

