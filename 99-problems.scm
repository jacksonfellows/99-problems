(use-modules (srfi srfi-1))

;;; 1
(define (my-last lst)
  (cond
   ((null? lst) (error "no last of empty list"))
   ((null? (cdr lst)) (car lst))
   (else (my-last (cdr lst)))))

;;; 2
(define (my-but-last lst)
  (cond
   ((null? lst) (error "no but last of empty list"))
   ((null? (cdr lst)) (error "no but last of single element list"))
   ((null? (cddr lst)) (car lst))
   (else (my-but-last (cdr lst)))))

;;; 3
(define (my-list-ref lst i)
  (cond
   ((negative? i) (error "index must be >= 0"))
   ((null? lst) (error "index too large"))
   ((zero? i) (car lst))
   (else (my-list-ref (cdr lst) (1- i)))))

;;; 4
(define (my-length lst)
  (fold (lambda (_ x) (1+ x)) 0 lst))

;;; 5
(define (my-reverse lst)
  (fold (lambda (a b) (append (list a) b)) '() lst))

;;; 6
(define (palindrome? lst)
  (equal? lst (my-reverse lst)))

;;; 7
(define (flatten lst)
  (append-map (lambda (x) (if (list? x) (flatten x) (list x))) lst))

;;; 8
(define (compress lst)
  (reverse! (fold (lambda (x acc) (if (and (not (null? acc)) (eq? x (car acc))) acc (cons x acc))) '() lst)))

;;; 9
(define (pack lst)
  (reverse! (fold (lambda (x acc) (if (and (not (null? acc)) (eq? x (caar acc))) (cons (cons x (car acc)) (cdr acc)) (cons (list x) acc))) '() lst)))

;;; 10
(define (encode lst)
  (map (lambda (xs) (list (length xs) (car xs))) (pack lst)))

;;; 11
(define (encode-modified lst)
  (map (lambda (xs) (if (null? (cdr xs)) (car xs) (list (length xs) (car xs)))) (pack lst)))

;;; 12
(define (n-copies x n)
  (unfold zero? (lambda (_) x) 1- n))

(define (decode-modified lst)
  (append-map (lambda (x) (if (list? x) (n-copies (cadr x) (car x)) (list x))) lst))

;;; 13
(define (encode-direct lst)
  (fold-right
   (lambda (x acc)
     (if (and (not (null? acc))
              (if (list? (car acc))
                  (eq? x (cadar acc))
                  (eq? x (car acc))))
         (cons (list (1+ (if (list? (car acc)) (caar acc) 1)) x) (cdr acc))
         (cons x acc)))
   '() lst))

;;; 14
(define (dupli lst)
  (fold-right (lambda (x acc) (cons x (cons x acc))) '() lst))

;;; 15
(define (repli lst n)
  (append-map (lambda (x) (n-copies x n)) lst))

;;; 16
(define (drop-every lst n)
  (reverse!
   (car (fold
         (lambda (x acc)
           (if (= n (cdr acc)) (cons (car acc) 1) (cons (cons x (car acc)) (1+ (cdr acc)))))
         '(() . 1) lst))))

;;; 17
(define (split lst n)
  (map reverse!
       (car (fold
             (lambda (x acc)
               (if (< (cdr acc) n)
                   (cons (list (cons x (caar acc)) (cadar acc)) (1+ (cdr acc)))
                   (cons (list (caar acc) (cons x (cadar acc))) (1+ (cdr acc)))))
             '((() ()) . 0) lst))))

;;; 18
(define (slice lst i k)
  (take (drop lst (1- i)) (1+ (- k i))))

;;; 19
(define (rotate lst n)
  (let ((_n (modulo n (length lst))))
    (append (drop lst _n) (take lst _n))))

;;; 20
(define (remove-at lst n)
  (append (take lst (1- n)) (drop lst n)))

;;; 21
(define (insert-at x lst n)
  (append (take lst (1- n)) (cons x (drop lst (1- n)))))

;;; 22
(define (range i k)
  (unfold (lambda (x) (>= x (1+ k))) values 1+ i))

;;; 23
(define (rnd-select lst n)
  (unfold zero? (lambda (_) (list-ref lst (random (length lst)))) 1- n))

;;; 24
(define (lotto-select n m)
  (unfold zero? (lambda (_) (random m)) 1- n))

;;; 25
(define (shuffle vec)
  (define (swap i)
    (let ((j (random (1+ i))))
      (let ((tmp (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j tmp))))
  (define (loop i)
    (when (not (zero? i))
      (swap i)
      (loop (1- i))))
  (loop (1- (vector-length vec)))
  vec)

(define (rnd-perm lst)
  (vector->list (shuffle (list->vector lst))))

;;; 26
(define (mapcdr f lst)
  (if (null? lst)
      '()
      (cons (f lst) (mapcdr f (cdr lst)))))

(define (combinations lst n)
  (cond
   ((zero? n) '())
   ((= 1 n) (map list lst))
   (else (apply append
                (mapcdr (lambda (tail)
                          (map (lambda (c) (cons (car tail) c))
                               (combinations (cdr tail) (1- n)))) lst)))))

;;; 27
(define (prep x)
  (lambda (a) (cons x a)))

(define (group lst ns)
  (if (null? ns)
      '(())
      (append-map
       (lambda (c)
         (map (prep c)
              (group (lset-difference eq? lst c) (cdr ns)))) (combinations lst (car ns)))))

;;; 28
(define (comparator f)
  (lambda (a b) (< (f a) (f b))))

(define (lsort lst)
  (sort lst (comparator length)))

(define (lfsort lst)
  (let ((freqs (map (lambda (l) (cons (cadr l) (car l))) (encode (sort (map length lst) <)))))
    (sort lst (comparator (lambda (x) (cdr (assq (length x) freqs)))))))

;;; 31
(define (isqrt x)
  (round (sqrt x)))

(define (is-prime x)
  (and (< 1 x)
       (not (or-map (lambda (n) (zero? (modulo x n))) (range 2 (isqrt x))))))

;;; 32
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (modulo a b))))

;;; 33
(define (coprime a b)
  (= 1 (gcd a b)))

;;; 34
(define (totient-phi m)
  (length (filter (lambda (r) (coprime r m)) (range 1 (1- m)))))

;;; 35
(define (prime-factors x)
  (if (<= x 1)
      '()
      (let ((rt (isqrt x)))
        (define (loop n)
          (cond
           ((< rt n) (list x))
           ((zero? (remainder x n)) (cons n (prime-factors (quotient x n))))
           (else (loop (1+ n)))))
        (loop 2))))

;;; 36
(define (prime-factors-mult x)
  (map (lambda (a) (list (cadr a) (car a))) (encode (prime-factors x))))

;;; 37
(define (totient-phi-improved m)
  (reduce * 1 (map (lambda (a) (* (1- (car a)) (expt (car a) (1- (cadr a))))) (prime-factors-mult m))))

;;; 39
(define (primes-range i k)
  (filter is-prime (range i k)))

;;; 40
(define (odd-pairs-sum-to n)
  (define (rec a b)
    (if (> a b)
        '()
        (cons (list a b) (rec (+ a 2) (- b 2)))))
  (rec 1 (1- n)))

(define (goldback n)
  (if (= n 4)
      (list 2 2)
      (find (lambda (a) (and-map is-prime a)) (odd-pairs-sum-to n))))

;;; 41
(use-modules (ice-9 format))

(define (goldback-list a b)
  (when (<= a b)
    (if (even? a)
        (begin (format #t "~a = ~?~%" a "~a + ~a" (goldback a)) (goldback-list (+ a 2) b))
        (goldback-list (1+ a) b))))

;;; 46
(define (cart-prod . lsts)
  (if (null? lsts)
      '(())
      (let ((prod (apply cart-prod (cdr lsts))))
        (append-map (lambda (x) (map (prep x) prod)) (car lsts)))))

(define (table f)
  (for-each
   (lambda (args) (format #t "~? ~a~%" "~a ~a" args (apply f args)))
   (cart-prod '(#t #f) '(#t #f))))

;;; 48
(define (tablen n f)
  (for-each
   (lambda (args) (format #t "~a ~a~%" (string-join (map (lambda (x) (if x "#t" "#f")) args)) (apply f args)))
   (apply cart-prod (map (lambda (_) '(#t #f)) (range 1 n)))))

;;; 49
(define (gray n)
  (if (= 1 n)
      '((0) (1))
      (let* ((old (gray (1- n)))
             (ref (reverse old)))
        (append (map (prep 0) old) (map (prep 1) ref)))))

;;; 50
(define (huffman-tree freqs)
  (if (= 1 (length freqs))
      (car freqs)
      (let* ((sorted (sort freqs (comparator cdr)))
             (a (car sorted))
             (b (cadr sorted))
             (parent (cons (list a b) (+ (cdr a) (cdr b)))))
        (huffman-tree (cons parent (cddr sorted))))))

(define (huffman-encoding tree)
  (let ((encoding '()))
    (define (rec tree path)
      (if (list? (car tree))
          (begin
            (rec (caar tree) (cons 0 path))
            (rec (cadar tree) (cons 1 path)))
          (set! encoding (cons (cons (car tree) (reverse path)) encoding))))
    (rec tree '())
    encoding))

(define huffman (compose huffman-encoding huffman-tree))

;;; 54
(define (tree? a)
  (if (list? a)
      (and (= 3 (length a)) (and-map tree? a))
      #t))

;;; 55
(define (leaf x)
  (list x #f #f))

(define (cbal-tree n)
  (case n
    ((0) '(#f))
    ((1) (list (leaf 'x)))
    (else (if (= 1 (remainder n 2))
              (let ((subtrees (cbal-tree (quotient (1- n) 2))))
                (map (prep 'x) (cart-prod subtrees subtrees)))
              (let* ((subtree1 (cbal-tree (quotient (1- n) 2)))
                     (subtree2 (cbal-tree (quotient n 2)))
                     (prod (cart-prod subtree1 subtree2)))
                (append (map (prep 'x) prod) (map (compose (prep 'x) reverse) prod)))))))

;;; 56
(define (mirror a b)
  (cond
   ((and (not a) (not b)) #t)
   ((and (list? a) (list? b)) (and (mirror (cadr a) (caddr b)) (mirror (caddr a) (cadr b))))
   (else #f)))

(define (symmetric t)
  (mirror t t))

;;; 57
(define (add x t)
  (if t
      (if (< x (car t))
          (list (car t) (add x (cadr t)) (caddr t))
          (list (car t) (cadr t) (add x (caddr t))))
      (leaf x)))

(define (construct lst)
  (fold add #f lst))

;;; 58
(define (sym-cbal-trees n)
  (filter symmetric (cbal-tree n)))

;;; 59
(define (hbal-tree x n)
  (cond
   ((<= n 0) '(#f))
   ((= n 1) (list (leaf x)))
   (else (let* ((n-1 (hbal-tree x (1- n)))
                (n-2 (hbal-tree x (- n 2)))
                (prod11 (cart-prod n-1 n-1))
                (prod12 (cart-prod n-1 n-2)))
           (append
            (map (prep x) prod12)
            (map (compose (prep x) reverse) prod12)
            (map (prep x) prod11))))))

;;; 60
(define (n-nodes t)
  (if t
      (+ 1 (n-nodes (cadr t)) (n-nodes (caddr t)))
      0))

(define (n-fibs-under n)
  (define (rec a b)
    (if (<= a n)
        (1+ (rec b (+ a b)))
        0))
  (rec 0 1))

(define (max-height n)
  (- (n-fibs-under n) 3))

(define (hbal-tree-nodes x n)
  (filter (lambda (t) (= n (n-nodes t)))
          (append-map (lambda (i) (hbal-tree x i)) (range 0 (max-height n)))))

;;; 61
(define (count-leaves t)
  (cond
   ((not t) 0)
   ((and-map not (cdr t)) 1)
   (else (+ (count-leaves (cadr t)) (count-leaves (caddr t))))))

(define (leaf? t)
  (and-map not (cdr t)))

(define (leaves t)
  (cond
   ((not t) '())
   ((leaf? t) (list (car t)))
   (else (append (leaves (cadr t)) (leaves (caddr t))))))

;;; 62
(define (internals t)
  (cond
   ((not t) '())
   ((leaf? t) '())
   (else (append (list (car t)) (internals (cadr t)) (internals (caddr t))))))

(define (at-level t n)
  (if t
      (if (= 1 n)
          (list (car t))
          (append (at-level (cadr t) (1- n)) (at-level (caddr t) (1- n))))
      '()))

;;; 63
(define (complete-binary-tree n)
  (case n
    ((0) #f)
    ((1) (leaf 'x))
    (else
     (let ((q (quotient (1- n) 2)))
       (list 'x (complete-binary-tree (- (1- n) q)) (complete-binary-tree q))))))

;;; 64-66: illustrations are not present

;;; 67
(define (print-tree t)
  (when t
    (display (car t))
    (when (not (leaf? t))
      (display "(")
      (print-tree (cadr t))
      (display ",")
      (print-tree (caddr t))
      (display ")"))))

(define (tree-to-string t)
  (with-output-to-string (lambda () (print-tree t))))

(define (end? s i)
  (or (<= (string-length s) i)
      (let ((c (string-ref s i)))
        (or (char=? c #\,) (char=? c #\))))))

(define (matchc c s i)
  (when (or (<= (string-length s) i) (not (char=? c (string-ref s i))))
    (error "expected" c)))

(use-modules (ice-9 receive))

(define (parse-tree s i)
  (cond
   ((end? s i) (values #f i))
   ((end? s (1+ i)) (values (leaf (string->symbol (substring s i (1+ i)))) (1+ i)))
   (else (let ((head (string->symbol (substring s i (1+ i)))))
           (matchc #\( s (1+ i))
           (receive (right i1)
               (parse-tree s (+ 2 i))
             (matchc #\, s i1)
             (receive (left i2)
                 (parse-tree s (1+ i1))
               (matchc #\) s i2)
               (values (list head right left) (1+ i2))))))))

(define (string-to-tree s)
  (receive (t i)
      (parse-tree s 0)
    (and (= i (string-length s)) t)))

;;; 68
(define (tree->preorder t)
  (if t
      (cons (car t) (append (tree->preorder (cadr t)) (tree->preorder (caddr t))))
      '()))

(define (tree->inorder t)
  (if t
      (append (tree->inorder (cadr t)) (cons (car t) (tree->inorder (caddr t))))
      '()))

(define (preorder->tree p)
  (if (null? p)
      #f
      (list (car p) #f (preorder->tree (cdr p)))))

(define (pre-in->tree p i)
  (if (or (null? p) (null? i))
      #f
      (receive (li ri)
          (break (lambda (x) (eq? x (car p))) i)
        (receive (lo ro)
            (split-at (cdr p) (length li))
          (list (car p) (pre-in->tree lo li) (pre-in->tree ro (cdr ri)))))))

;;; 69
(define (print-ds t)
  (if t
      (begin
        (display (car t))
        (print-ds (cadr t))
        (print-ds (caddr t)))
      (display ".")))

(define (tree->ds t)
  (with-output-to-string (lambda () (print-ds t))))

(define (parse-ds s i)
  (cond
   ((<= (string-length s) i) (values #f i))
   ((char=? #\. (string-ref s i)) (values #f (1+ i)))
   (else (receive (l i1)
             (parse-ds s (1+ i))
           (receive (r i2)
               (parse-ds s i1)
             (values
              (list (string->symbol (substring s i (1+ i))) l r)
              i2))))))

(define (ds->tree s)
  (receive (t i)
      (parse-ds s 0)
    (and (= i (string-length s)) t)))

;;; 70
(define n-nodes-multiway (compose length flatten))

(define (multi->string t)
  (define (rec t)
    (if (list? t)
        (case (length t)
          ((1) (rec (car t)))
          ((2) (rec (car t)) (rec (cadr t)) (display "^"))
          (else (rec (car t))
                (rec (cadr t))
                (for-each (lambda (x) (display "^") (rec x)) (cddr t))
                (display "^")))
        (display t)))
  (with-output-to-string (lambda () (rec t))))

(define (add-at-d x t d)
  (if (zero? d)
      (append (if (list? t) t (list t)) (list x))
      (append (drop-right t 1) (list (add-at-d x (last t) (1- d))))))

(define (sf c acc)
  (let ((t (car acc))
        (d (cdr acc)))
    (if (char=? #\^ c)
        (cons t (1- d))
        (cons (add-at-d (string->symbol (string c)) t d) (if (null? t) d (1+ d))))))

(define (string->multi s)
  (let* ((acc (string-fold sf '(() . 0) s))
         (t (car acc))
         (d (cdr acc)))
    (when (zero? d) t)))

;;; 71
(define (ipl t)
  (define (rec d t)
    (if (list? t)
        (+ d (apply + (map (lambda (x) (rec (1+ d) x)) (cdr t))))
        d))
  (rec 0 t))

;;; 72
(define (bottom-up t)
  (if (list? t)
      (append (append-map bottom-up (cdr t)) '() (list (car t)))
      (list t)))

;;; 81
(define (graph-refs x g)
  (if (null? g)
      '()
      (if (eqv? x (caar g))
          (cons (cdar g) (graph-refs x (cdr g)))
          (graph-refs x (cdr g)))))

(define (assq-remove-pair p g)
  (if (null? g)
      '()
      (if (equal? p (car g))
          (assq-remove-pair p (cdr g))
          (cons (car g) (assq-remove-pair p (cdr g))))))

(define (paths start end g)
  (if (eqv? start end)
      (list (list end))
      (append-map (lambda (new-start) (map (prep start) (paths new-start end (assq-remove-pair (cons start new-start) g)))) (graph-refs start g))))

;;; 82
(define (cycle start g)
  (append-map (lambda (new-start) (map (prep start) (paths new-start start (assq-remove-pair (cons start new-start) g)))) (graph-refs start g)))

;;; 83
(define (spanning-trees g)
  #f)
