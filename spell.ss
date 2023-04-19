
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2023                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

(define initkey 5077)

(define applyhashlist
  (lambda (hashlist w)
    (if (null? hashlist)
        '()
        (cons ((car hashlist) w) (applyhashlist (cdr hashlist) w)))))

(define applyhashlist-dict
  (lambda (hashlist dict)
    (if (null? dict)
        '()
        (cons (applyhashlist hashlist (car dict)) (applyhashlist-dict hashlist (cdr dict))))))

(define list-equal
  (lambda (x y)
    (cond
      ((and (null? x) (null? y)) #t)
      ((or (null? x) (null? y)) #f)
      ((= (car x) (car y)) (list-equal (cdr x) (cdr y)))
      (else #f))))

(define contains-list
  (lambda (l d)
    (cond
      ((null? d) #f)
      ((eq? #t (list-equal l (car d))) #t)
      (else (contains-list l (cdr d))))))

;; -----------------------------------------------------
;; KEY FUNCTION

;;(define key
;;  (lambda (w)
;;    (cond
;;      ((null? w) initkey)
;;      (else (+ (ctv (car w)) (* 29 (key (cdr w))))))
;;))

(define key
  (lambda (w)
    (reduce
     (lambda (x y)
       (+ (ctv x) (* 29 y))) w initkey)))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 104146015601
;;   (key '(m a y))           = 123844020
;;   (key '(t r e e f r o g)) = 2539881083658035

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
      (modulo (key w) size)
)))

;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17
(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
      (floor (* size (- (* (key w) A) (floor (* (key w) A)))))
)))

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 51317
;;  (hash-1 '(m a y))           ==> 27994
;;  (hash-1 '(t r e e f r o g)) ==> 33645
;;
;;  (hash-2 '(h e l l o))       ==> 47249
;;  (hash-2 '(m a y))           ==> 8148
;;  (hash-2 '(t r e e f r o g)) ==> 53006
;;
;;  (hash-3 '(h e l l o))       ==> 4322.0
;;  (hash-3 '(m a y))           ==> 3288.0
;;  (hash-3 '(t r e e f r o g)) ==> 0.0
;;
;;  (hash-4 '(h e l l o))       ==> 538.0
;;  (hash-4 '(m a y))           ==> 409.0
;;  (hash-4 '(t r e e f r o g)) ==> 0.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (lambda (w)
      (define word-hash (applyhashlist hashfunctionlist w))
      (define dict-hash (applyhashlist-dict hashfunctionlist dict))
      ;; (display word-hash)
      ;; (display "\n")
      ;; (display dict-hash)
      (contains-list word-hash dict-hash)
)))

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #f  

