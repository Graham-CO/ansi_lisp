;;;; Function that implements naive BFS of a network

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
;; Naive BFS - track paths taken to return shortest trace (not just the target)
;;           - guarantees (one of) shortest paths
(defun bfs (end queue net)
  (if (null queue)
      nil ; if queue empty, return nil to mark end
      (let ((path (car queue))) ; save the head of path
        (let ((node (car path)))
          (if (eql node end)      ; if path is found
              (reverse path)      ; reverse to show start -> end
              (bfs end            ; else recursively call bfs with rest of queue
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
                (cons n path))
            (cdr (assoc node net))))