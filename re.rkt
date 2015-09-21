#lang racket/base

(require racket/function racket/list racket/match)

(define (re-match? rules input)
  (let-values ([(matches? rest-input) (re-match rules (string->list input))])
    matches?))

(define (re-match rules input #:prev-match? [prev-match? #f])
  (if (empty? rules)
      (if (empty? input)
          (values #t input)
          (values #f input))
      (if (empty? input)
          ;; rules is not empty, so check if the remaining rules are satisfied
          (cond
            [(re-nullable? rules) (values #t input)]
            [(and prev-match? (equal? '+ (caar rules)) (re-nullable? (rest rules))) (values #t input)]
            [else (values #f input)])
          (match (first rules)
            [(list '= operand) (if (re-match-operand? operand (first input))
                                   (re-match (rest rules) (rest input))
                                   (values #f input))]
            [(list '? operand) (if (re-match-operand? operand (first input))
                                   (re-match (rest rules) (rest input))
                                   (re-match (rest rules) input))]
            [(list '+ operand) (if (re-match-operand? operand (first input))
                                   (re-match (rest rules) (re-consume-char (first input) input) #:prev-match? #t)
                                   (values #f input))]
            [(list '* operand) (re-match (rest rules) (re-consume-star operand (first input) input))]
            [(list 'disj (? list? a) (? list? b)) (let-values ([(matches? new-input) (re-match a input)])
                                                    (if matches?
                                                        (re-match (rest rules) new-input)
                                                        (re-match b input)))]
            [operator (displayln (format "Invalid operator: ~s" operator)) (values 'poop input)]))))

;; Return true if the rest of the rules are nullable; otherwise false.
(define (re-nullable? rules)
  (cond
    [(empty? rules) #t]
    [(or (equal? '? (caar rules)) (equal? '* (caar rules))) (re-nullable? (rest rules))]
    [else #f]))

;; TODO: The drop function needs to work on all operands, not just chars.
(define (re-consume-char drop-char input)
  (dropf input (curry char=? drop-char)))

(define (re-consume-star operand drop-char input)
  (if (re-match-operand? operand drop-char)
      (re-consume-char drop-char input)
      input))

(define (re-match-operand? operand input-char)
  (match operand
    [(? char? operand-char) (char=? operand-char input-char)]
    [(list 'one-of operand-chars ..1) (ormap (curry char=? input-char) operand-chars)]
    [(list 'none-of operand-chars ..1) (not (ormap (curry char=? input-char) operand-chars))]
    [(list 'range (? char? start) (? char? end)) (re-match-operand? (cons 'one-of (char-range start end)) input-char)]
    ['any #t]
    ['d (re-match-operand? '(range #\0 #\9) input-char)]
    ;; TODO: Add all the other aliased character classes
    [operand (displayln (format "Invalid operand: ~s" operand)) #f]))

(define (char-range start end)
  (map integer->char (range (char->integer start) (add1 (char->integer end)))))

(module+ test
  (require rackunit)

  ;; required
  ;l; 
  (let ([ex '((= #\o) (= #\w) (= #\e)(= #\n))])
    (check-true (re-match? ex "owen"))
    (check-false (re-match? ex "owe"))
    (check-false (re-match? ex "ow"))
    (check-false (re-match? ex "o"))
    (check-false (re-match? ex ""))
    (check-false (re-match? ex "owens"))
    (check-false (re-match? ex "asdf")))

  ;;; optional
  
  (let ([ex '((= #\o) (? #\w) (= #\e) (= #\n))])
    (check-true (re-match? ex "owen"))
    (check-true (re-match? ex "oen"))
    (check-false (re-match? ex "owewns")))

  ;;; kleene plus
  
  (let ([ex '((= #\o) (= #\w) (+ #\e) (= #\n))])
    (check-true (re-match? ex "owen"))
    (check-true (re-match? ex "oween"))
    (check-true (re-match? ex "oweeeeeeen"))
    (check-false (re-match? ex "own"))
    (check-false (re-match? ex "ow"))
    (check-false (re-match? ex "owe")))

  (let ([ex '((= #\o) (= #\w) (+ #\e))])
    (check-true (re-match? ex "owe"))
    (check-true (re-match? ex "owee"))
    (check-true (re-match? ex "oweeeeee"))
    (check-false (re-match? ex "ow"))
    (check-false (re-match? ex "owen")))

  (let ([ex '((+ #\e))])
    (check-true (re-match? ex "e"))
    (check-true (re-match? ex "ee"))
    (check-true (re-match? ex "eeeeee"))
    (check-false (re-match? ex ""))
    (check-false (re-match? ex "en"))
    (check-false (re-match? ex "n")))

  ;;; kleene star

  (let ([ex '((= #\o) (= #\w) (* #\e) (= #\n))])
    (check-true (re-match? ex "owen"))
    (check-true (re-match? ex "oween"))
    (check-true (re-match? ex "oweeeeeeen"))
    (check-true (re-match? ex "own"))
    (check-false (re-match? ex "ow"))
    (check-false (re-match? ex "owe")))

  (let ([ex '((= #\o) (= #\w) (* #\e))])
    (check-true (re-match? ex "owe"))
    (check-true (re-match? ex "owee"))
    (check-true (re-match? ex "oweeeeee"))
    (check-true (re-match? ex "ow"))
    (check-false (re-match? ex "owen")))

  (let ([ex '((* #\e))])
    (check-true (re-match? ex "e"))
    (check-true (re-match? ex "ee"))
    (check-true (re-match? ex "eeeeee"))
    (check-true (re-match? ex ""))
    (check-false (re-match? ex "en"))
    (check-false (re-match? ex "n")))

  (let ([ex '((* #\e) (= #\n))])
    (check-true (re-match? ex "en"))
    (check-true (re-match? ex "een"))
    (check-true (re-match? ex "eeeeen"))
    (check-true (re-match? ex "n"))
    (check-false (re-match? ex "")))

  ;; character classes
  
  (let ([ex '((= #\o) (= (one-of #\w #\e)) (= #\n))])
    (check-true (re-match? ex "own"))
    (check-true (re-match? ex "oen"))
    (check-false (re-match? ex "on")))

  (let ([ex '((= #\o) (= (range #\a #\c)) (= #\n))])
    (check-true (re-match? ex "oan"))
    (check-true (re-match? ex "obn"))
    (check-true (re-match? ex "ocn"))
    (check-false (re-match? ex "odn"))
    (check-false (re-match? ex "on"))
    (check-false (re-match? ex "oaan")))

  (let ([ex '((= any))])
    (check-true (re-match? ex "a"))
    (check-false (re-match? ex "aa"))
    (check-false (re-match? ex "")))

  (let ([ex '((= d))])
    (check-true (re-match? ex "0"))
    (check-true (re-match? ex "9"))
    (check-false (re-match? ex "a")))

  (let ([ex '((= (one-of #\a #\b #\c)))])
    (check-false (re-match? ex "d"))
    (check-false (re-match? ex "3"))
    (check-true (re-match? ex "a"))
    (check-true (re-match? ex "b"))
    (check-true (re-match? ex "c")))
  
  (let ([ex '((= (none-of #\a #\b #\c)))])
    (check-true (re-match? ex "d"))
    (check-true (re-match? ex "3"))
    (check-false (re-match? ex "a"))
    (check-false (re-match? ex "b"))
    (check-false (re-match? ex "c")))

  ;; disjunction
  
  (let ([ex '((disj ((= #\a)) ((= #\b))))])
    (check-true (re-match? ex "a"))
    (check-true (re-match? ex "b"))
    (check-false (re-match? ex "c"))
    (check-false (re-match? ex ""))
    (check-false (re-match? ex "ac"))
    (check-false (re-match? ex "ab"))
    (check-false (re-match? ex "ba")))

  (let ([ex '((disj ((= #\a) (= #\b)) ((= #\c))))])
    (check-true (re-match? ex "ab"))
    (check-true (re-match? ex "c"))
    (check-false (re-match? ex "abc"))
    (check-false (re-match? ex ""))
    (check-false (re-match? ex "ac"))
    (check-false (re-match? ex "bc")))
  
  )
