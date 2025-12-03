(defun prime-factors (n)
  "return list of prime factors of n (with duplicates)."
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
  "return sorted list of unique prime factors of n."
  (remove-duplicates (prime-factors n)))

(defun totient (n)
  "euler's totient function phi(n)."
  (if (<= n 1)
      1
      (let ((result n)
            (primes (unique-prime-factors n)))
        (dolist (p primes result)
          (setf result (- result (/ result p)))))))

(defun divisors (n)
  "return list of all divisors of n."
  (loop for i from 1 to n
        when (zerop (mod n i))
        collect i))

;;OEIS a322366

(defun can-express-as-sum (target primes memo)
  "check if target can be expressed as sum of primes (frobenius coin problem)."
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
  "check if k test tubes can be balanced in n holes."
  (let ((memo (make-hash-table :test 'equal)))
    (and (can-express-as-sum k primes memo)
         (can-express-as-sum (- n k) primes memo))))

(defun a322366-single (n)
  "calculate a322366(n) - number of balanced configurations."
  (cond
    ((= n 0) 1)  ; k=0 works
    ((= n 1) 0)  ; no balanced config
    ((= n 2) 2)  ; k=0,2 work
    ((= n 3) 2)  ; k=0,3 work
    ((= n 4) 3)  ; k=0,2,4 work
    (t ; n >= 5: use ishwaran's theorem
     (let ((primes (unique-prime-factors n))
           (count 0))
       ;; k=0 and k=n always work
       (incf count 2)
       ;; check k from 1 to n-1
       (loop for k from 1 to (- n 1)
             when (can-balance k n primes)
             do (incf count))
       count))))

(defun compute-centrifuge-range (start end results)
  "compute a322366 for range [start, end] and store in results array."
  (loop for n from start to end
        do (setf (aref results n) (a322366-single n)))
  results)

;; farey sequence cardinality

(defun farey-cardinality-standard (n)
  "calculate |f_n| using the standard formula: 1 + sum phi(k)."
  (1+ (loop for k from 1 to n
            sum (totient k))))

(defun compute-farey-range-optimized (start end results)
  "compute farey cardinality incrementally for range [start, end]."
  
  ;; 1. compute the baseline for the number immediately preceding 'start'
  ;;    if start is 0, the sum is 1. if start > 0, we calculate f_(start-1).
  (let ((current-sum (if (zerop start) 
                         1 
                         (farey-cardinality-standard (1- start)))))
    
    ;; 2. handle the 0 case specifically if it is in range
    (when (zerop start)
      (setf (aref results 0) 1)
      (setf current-sum 1))

    ;; 3. loop through range. for farey, f_n = f_(n-1) + phi(n).
    (loop for n from (if (zerop start) 1 start) to end
          do (let ((phi (totient n)))
               (incf current-sum phi)
               (setf (aref results n) current-sum))))
  results)

;; testing against conjecture

(defun farey-via-conjecture (n centrifuge-results)
  "compute |F_n| using conjecture: sum from i=1 to n of [(i+1) - a(i)]
   where a(i) is the centrifuge sequence (1-indexed)."
  (if (zerop n)
      1  ; |F_0| = 1
      (loop for i from 1 to n
            sum (- (+ i 1) (aref centrifuge-results i)))))

(defun verify-conjecture (&optional (max-n 100))
  "verify the conjecture: |F_n| = sum_{i=1}^{n} [(i+1) - a(i)]"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "  CONJECTURE VERIFICATION~%")
  (format t "  |F_n| = Σ(i=1 to n) [(i+1) - a(i)] where a(i) is A322366~%")
  (format t "================================================================================~%~%")
  
  ;; compute both sequences
  (format t "Computing centrifuge sequence (A322366)...~%")
  (let ((centrifuge (time (run-parallel-job :centrifuge max-n 8))))
    (format t "~%Computing Farey cardinality (standard method : totient fn)...~%")
    (let ((farey-standard (time (run-parallel-job :farey max-n 8))))
      
      ;; compute via conjecture
      (format t "~%Computing Farey cardinality (via conjectured sum)...~%")
      (let* ((farey-conjecture (make-array (1+ max-n) :initial-element 0))
             (differences (make-array (1+ max-n) :initial-element 0))
             (start-time (get-internal-real-time)))
        
        (setf (aref farey-conjecture 0) 1)
        (setf (aref differences 0) 0)
        (loop for n from 1 to max-n
              do (setf (aref farey-conjecture n)
                      (farey-via-conjecture n centrifuge))
                 (setf (aref differences n)
                      (abs (- (aref farey-standard n) (aref farey-conjecture n)))))
        
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
          (format t "Evaluation took:~%  ~,6f seconds of real time~%" elapsed))
        
        ;; verification
        (format t "~%~%--- VERIFICATION RESULTS ---~%~%")
        (let ((all-match t)
              (mismatches '()))
          (loop for n from 0 to max-n
                do (unless (= (aref farey-standard n) (aref farey-conjecture n))
                     (setf all-match nil)
                     (push n mismatches)))
          
          (if all-match
              (format t "✓ CONJECTURE VERIFIED for all n ∈ [0, ~d]~%~%" max-n)
              (format t "✗ MISMATCHES found at n = ~a~%~%" (nreverse mismatches)))
          
          ;; show sample values
          (format t "Sample comparison (first 20 terms):~%~%")
          (format t "  n   a(n)  Standard  Conjecture  (n+1)-a(n)  |Diff|~%")
          (format t " ---  ----  --------  ----------  ----------  ------~%")
          (loop for n from 0 to (min 19 max-n)
                do (format t " ~2d   ~4d    ~5d      ~5d         ~3d      ~3d~%"
                          n
                          (aref centrifuge n)
                          (aref farey-standard n)
                          (aref farey-conjecture n)
                          (if (zerop n) 
                              0 
                              (- (+ n 1) (aref centrifuge n)))
                          (aref differences n)))
          
          (format t "~%Last 10 terms:~%~%")
          (format t "  n   a(n)  Standard  Conjecture  |Diff|  Match?~%")
          (format t " ---  ----  --------  ----------  ------  ------~%")
          (loop for n from (max 0 (- max-n 9)) to max-n
                do (format t " ~3d  ~4d    ~5d      ~5d      ~4d    ~a~%"
                          n
                          (aref centrifuge n)
                          (aref farey-standard n)
                          (aref farey-conjecture n)
                          (aref differences n)
                          (if (= (aref farey-standard n) (aref farey-conjecture n))
                              "✓"
                              "✗")))
          
          ;; output difference sequence for OEIS lookup
          (format t "~%~%--- DIFFERENCE SEQUENCE FOR OEIS LOOKUP ---~%~%")
          (format t "Absolute differences |F_n - Conjecture_n|:~%~%")
          (format t "~a~%" (coerce differences 'list))

          ;; analyze the error sequence
          (format t "~%~%--- ERROR SEQUENCE ANALYSIS ---~%~%")
          (format t "Jump points (where error increases):~%~%")
          (format t "  n   Error  Jump  a(n)  φ(n)  d(n)  σ(n)  ω(n)  Ω(n)~%")
          (format t " ---  -----  ----  ----  ----  ----  ----  ----  ----~%")
          (loop for n from 1 to max-n
                when (> (aref differences n) (aref differences (1- n)))
                do (let* ((jump (- (aref differences n) (aref differences (1- n))))
                         (phi (totient n))
                         (d (length (divisors n)))
                         (sigma (reduce #'+ (divisors n)))
                         (omega (length (unique-prime-factors n)))
                         (big-omega (length (prime-factors n))))
                     (format t " ~3d  ~5d  ~4d  ~4d  ~4d  ~4d  ~4d  ~4d  ~4d~%"
                            n (aref differences n) jump (aref centrifuge n)
                            phi d sigma omega big-omega)))
          ;; try to find pattern in jumps
          (format t "~%~%Analyzing jump magnitudes:~%~%")
          (let ((jumps '()))
            (loop for n from 1 to max-n
                  when (> (aref differences n) (aref differences (1- n)))
                  do (push (list n (- (aref differences n) (aref differences (1- n)))) jumps))
            (setf jumps (nreverse jumps))
            (format t "n -> jump: ~{~a~^, ~}~%" jumps))
          
          (values all-match farey-standard farey-conjecture centrifuge differences))))))


(defun run-parallel-job (job-type max-n num-threads)
  "generic parallel driver for both sequences."
  (let* ((results (make-array (1+ max-n) :initial-element 0))
         (chunk-size (ceiling (1+ max-n) num-threads))
         (threads '()))
    
    (loop for i from 0 below num-threads
          for start = (* i chunk-size)
          for end = (min (+ start chunk-size -1) max-n)
          when (<= start max-n)
          do (let ((t-start start)
                   (t-end end))
               (push (sb-thread:make-thread
                      (lambda ()
                        (case job-type
                          (:centrifuge (compute-centrifuge-range t-start t-end results))
                          (:farey (compute-farey-range-optimized t-start t-end results))))
                      :name (format nil "worker-~d" i))
                     threads)))
    
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    results))


(defun test-centrifuge (&optional (max-n 100))
  (format t "~%--- a322366 (centrifuge) [0..~d] ---~%" max-n)
  (let ((results (time (run-parallel-job :centrifuge max-n 8))))
    (format t "first few: ~a~%" (subseq results 0 (min 20 (length results))))
    results))

(defun test-farey (&optional (max-n 100))
  (format t "~%--- farey cardinality [0..~d] ---~%" max-n)
  (let ((results (time (run-parallel-job :farey max-n 8))))
    (format t "first few: ~a~%" (subseq results 0 (min 20 (length results))))
    results))

(defun run-all-tests ()
  (test-centrifuge 100)
  (test-farey 100)
  (verify-conjecture 100))

(run-all-tests)
