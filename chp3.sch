;3.1
(define (make-accumulator amt)
  (lambda (x)
    (begin (set! amt (+ amt x)))
           amt))
;3.2
(define (make-monitored f)
  (let ((num-calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) num-calls)
            (else
              (begin (set! num-calls (+ 1 num-calls))
                     (f arg)))))))
;3.3
(define (make-account pw balance)
  (let ((consecutive-attempts 0))
    (define (call-the-cops . args) (error "I called the cops"))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
            balance)
    (define (check-password pass)
      (lambda (f)
        (if (eq? pass pw)
            f
            (if (> consecutive-attempts 5)
                call-the-cops
                (begin (set! consecutive-attempts (+ 1 consecutive-attempts))
                       (error "Incorrect password"))))))
    (define (dispatch pass m)
      (cond ((eq? m 'withdraw)
                  ((check-password pass) withdraw))
            ((eq? m 'deposit)
                  ((check-password pass) deposit))
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))
;3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (integral-eq)
    (< (+ (expt (- (random-in-range x1 x2) (car P)) 2)
          (expt (- (random-in-range y1 y2) (cadr P)) 2))
       (expt (/ (- x2 x1) 2) 2)))
  (monte-carlo trials integral-eq))

(define P (list 5 7))

(define (estimate-pi P x1 x2 y1 y2 trials)
  (/ (* (estimate-integral P x1 x2 y1 y2 trials)
        (* (- x2 x1) (- y2 y1)))
     (expt (/ (- x2 x1) 2) 2)))
