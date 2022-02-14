;;;; Utility for operating on sorted Vectors
;;; Binary Search of a Sorted Vector

;; Search controller
(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len)) ; if vec empty and will return F & terminate
         (finder obj vec 0 (- len 1))))) ; else call finder 

;; Performs actual search
(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range) ; if range is empty 
        (if (eql obj (aref vec start)) ; if object is found
            obj                        ; return it
            nil)                       ; else return nil (not in vec)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2) ; if number looking at is greater than target
                (finder obj vec start (- mid 1)) ; search to left
                (if (> obj obj2) ; if number looking at is less than target
                    (finder obj vec (+ mid 1) end) ; search to right
                    obj)))))))

