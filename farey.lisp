(defun prime-factors (n)
  "Return list of prime factors of n (with multiplicity)"
  (loop with factors = '()
        with d = 2
        while (> n 1)
        do (if (zerop (mod n d))
               (progn
                 (push d factors)
                 (setf n (/ n d)))
               (setf d (if (= d 2) 3 (+ d 2))))
        finally (return (nreverse factors))))

(defun unique-prime-factors (n)
  "Return sorted list of unique prime factors of n"
  (remove-duplicates (prime-factors n)))

(defun can-express-as-sum (target primes memo)
  "Check if target can be expressed as sum of primes (with repetition).
   Uses memoization for efficiency."
  (cond
    ((zerop target) t)
    ((minusp target) nil)
    ((gethash target memo) (gethash target memo))
    (t (let ((result
              (some (lambda (p)
                      (can-express-as-sum (- target p) primes memo))
                    primes)))
         (setf (gethash target memo) result)
         result))))

(defun can-balance (k n primes)
  "Check if k test tubes can be balanced in n holes using Ishwaran's theorem"
  (let ((memo (make-hash-table :test 'equal)))
    (and (can-express-as-sum k primes memo)
         (can-express-as-sum (- n k) primes memo))))

(defun a322366-single (n)
  "Calculate A322366(n) - number of balanced configurations"
  (cond
    ((= n 0) 1)  ; k=0 works
    ((= n 1) 0)  ; no balanced config
    ((= n 2) 2)  ; k=0,2 work
    ((= n 3) 2)  ; k=0,3 work
    ((= n 4) 3)  ; k=0,2,4 work
    (t ; n >= 5: use Ishwaran's theorem
     (let ((primes (unique-prime-factors n))
           (count 0))
       ;; k=0 and k=n always work
       (incf count 2)
       ;; Check k from 1 to n-1
       (loop for k from 1 to (- n 1)
             when (can-balance k n primes)
             do (incf count))
       count))))

(defun compute-range (start end)
  "Compute A322366 for range [start, end] and print results"
  (loop for n from start to end
        do (let ((value (a322366-single n)))
             (format t "a(~d) = ~d~%" n value)
             (force-output))))

(defun a322366-parallel (max-n &optional (num-threads 32))
  "Calculate A322366(0) through A322366(max-n) using multiple threads"
  (let* ((chunk-size (ceiling (1+ max-n) num-threads))
         (threads '()))
    
    ;; Create threads for each chunk
    (loop for i from 0 below num-threads
          for start = (* i chunk-size)
          for end = (min (+ start chunk-size -1) max-n)
          when (<= start max-n)
          do (push (sb-thread:make-thread
                    (lambda ()
                      (compute-range start end))
                    :name (format nil "Worker-~d" i))
                   threads))
    
    ;; Wait for all threads to complete
    (dolist (thread threads)
      (sb-thread:join-thread thread))))

;; Run it immediately
(format t "Computing A322366 series with 32 threads...~%~%")
(time (a322366-parallel 1000 32))
(format t "~%Done!~%")
