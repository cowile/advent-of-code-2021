#!/usr/bin/guile \
-e main -s
!#

(use-modules
 (ice-9 textual-ports)
 (ice-9 getopt-long)
 (srfi srfi-1)
 (srfi srfi-8)
 (srfi srfi-9))

;; High level solution. Read input file and delegate details to
;; appropriate function.
(define (main args)
  (let*
      ((option-spec
	'((input (single-char #\i) (required? #t) (value #t))
	  (challenge (single-char #\c) (required? #t) (value #t))))
       (options (getopt-long args option-spec))
       (input (read-file (option-ref options 'input #f)))
       (challenge (string->number (option-ref options 'challenge #f)))
       (solution (advent-of-code challenge input)))
    (simple-format
     (current-output-port)
     "Part 1 Solution: ~A.\nPart 2 Solution: ~A.\n"
     (first solution)
     (second solution))))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (port)
      (get-string-all port))))

(define (advent-of-code challenge input)
  ((vector-ref (solutions) (1- challenge)) input))

(define (solutions)
  (vector
   solution-1:sonar-sweep
   solution-2:dive
   solution-3:binary-diagnostic))

;; Solution utilities.
(define (split-lines string)
  (string-split (string-trim-both string) #\newline))

(define (strings->numbers strings)
  (map string->number strings))

(define (sum numbers) (apply + numbers))

;; Solutions.
(define (count-increases measurements)
  (count < (drop-right measurements 1) (drop measurements 1)))

(define (sliding-windows measurements window)
  (if
   (< (length measurements) window)
   '()
   (cons (take measurements window) (sliding-windows (cdr measurements) window))))

(define (solution-1:sonar-sweep input)
  (let*
      ((depth-measurements (strings->numbers (split-lines input)))
       (increases (count-increases depth-measurements))
       (windows (sliding-windows depth-measurements 3))
       (window-sums (map sum windows))
       (sum-increases (count-increases window-sums)))
    (list increases sum-increases)))

(define-record-type <submarine>
  (make-submarine distance depth aim)
  submarine?
  (distance submarine-distance)
  (depth submarine-depth)
  (aim submarine-aim))

(define-record-type <submarine-command>
  (make-submarine-command distance depth)
  submarine-command?
  (distance submarine-command-distance)
  (depth submarine-command-depth))
 
(define (line->submarine-command line)
  (let*
      ((split-line (string-split line #\space))
       (direction (string->symbol (first split-line)))
       (magnitude (string->number (second split-line))))
    (case direction
      ((forward) (make-submarine-command magnitude 0))
      ((down) (make-submarine-command 0 magnitude))
      ((up) (make-submarine-command 0 (- magnitude))))))

(define (apply-submarine-command-1 command submarine)
  (make-submarine
   (+ (submarine-distance submarine) (submarine-command-distance command))
   (+ (submarine-depth submarine) (submarine-command-depth command))
   0))

(define (apply-submarine-command-2 command submarine)
  (let
      ((new-aim (+ (submarine-aim submarine) (submarine-command-depth command))))
  (make-submarine
   (+ (submarine-distance submarine) (submarine-command-distance command))
   (+ (submarine-depth submarine) (* new-aim (submarine-command-distance command)))
   new-aim)))

(define (submarine-magnitude submarine)
  (* (submarine-distance submarine) (submarine-depth submarine)))

(define (solution-2:dive input)
  (let*
      ((lines (split-lines input))
       (commands (map line->submarine-command lines))
       (initial-position (make-submarine 0 0 0))
       (final-position-1 (fold apply-submarine-command-1 initial-position commands))
       (final-magnitude-1 (submarine-magnitude final-position-1))
       (final-position-2 (fold apply-submarine-command-2 initial-position commands))
       (final-magnitude-2 (submarine-magnitude final-position-2)))
    (list final-magnitude-1 final-magnitude-2)))

(define (line->bit-list line)
  (map
   (lambda (x) (- (char->integer x) (char->integer #\0)))
   (string->list line)))

(define (bit-list->integer bit-list)
  (apply
   +
   (map
    (lambda (x y) (* x (expt 2 y)))
    (reverse bit-list)
    (iota (length bit-list)))))

(define (health-safety-rating position most-common? candidates)
  (if
   (<= (length candidates) 1)
   (first candidates)
   (receive (zeroes ones)
       (partition (lambda (x) (= (list-ref x position) 0)) candidates)
     (let
	 ((zeroes-len (length zeroes))
	  (ones-len (length ones)))
       (health-safety-rating
	(1+ position)
	most-common?
	(if
	 most-common?
	 (if (>= ones-len zeroes-len) ones zeroes)
	 (if (<= zeroes-len ones-len) zeroes ones)))))))

(define (solution-3:binary-diagnostic input)
  (let*
      ((lines (split-lines input))
       (numbers (length lines))
       (bit-lists (map line->bit-list lines))
       (bits (length (first bit-lists)))
       (position-sums (apply map + bit-lists))
       (gamma-bits (map (lambda (x) (if (>= (* 2 x) numbers) 1 0)) position-sums))
       (gamma (bit-list->integer gamma-bits))
       (epsilon-bits (map (lambda (x) (if (>= (* 2 x) numbers) 0 1)) position-sums))
       (epsilon (bit-list->integer epsilon-bits))
       (power (* gamma epsilon))
       (oxygen-rating (bit-list->integer (health-safety-rating 0 #t bit-lists)))
       (co2-rating (bit-list->integer (health-safety-rating 0 #f bit-lists)))
       (health-rating (* oxygen-rating co2-rating)))
    (list power health-rating)))
