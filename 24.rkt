(require picturing-programs)
(require "posn-util.rkt")
(define (distance x1 y1 x2 y2) (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))



(define catimage (scale 0.2 (bitmap/url https://raw.githubusercontent.com/MasonQiao/Ch-24-Universe-Project/main/cat.png)))
(define mouseimage (scale 0.2 (bitmap/url https://raw.githubusercontent.com/MasonQiao/Ch-24-Universe-Project/main/mouse.png)))

(define floor (bitmap/url https://raw.githubusercontent.com/MasonQiao/Ch-24-Universe-Project/main/floor.png))


(define scorelist (list 1 2 5 10 20 50 100 200 500 1000 2000 5000 10000))
(define (aux dummy) (if (= (random 2) 1) true false))
(define (score l)
  (cond [(empty? l) 1000000000]
        [else (if aux (first l) (score (rest l)))]))


(define-struct p (m cx cy mx my cs ms t ct mt))
;BASE MODEL: (make-p (p-m m) (p-cx m) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m))


(define (dh m) (cond
                 [(> (p-ct) 0) (overlay/align "middle" "top" (text "Mouse was caught. Cat wins." 20 "black") (place-image mouseimage (p-mx) (p-my) (place-image catimage (p-cx) (p-cy) floor)))]
                 [(> (p-mt) 0) (overlay/align "middle" "top" (text "Time ran out. Mouse wins." 20 "black") (place-image mouseimage (p-mx) (p-my) (place-image catimage (p-cx) (p-cy) floor)))]
                 [else (overlay/align "middle" "top" (text (string-append "Time Left: " (number->string (p-t m))) 20 "black") (place-image mouseimage (p-mx) (p-my) (place-image catimage (p-cx) (p-cy) floor)))]))

(define (th m) (if (<= (p-t 0)) mwins (if (< (distance (p-cx m) (p-cy m) (p-mx m) (p-my m)) 100) cwins m)))
(define (cwins m) (make-p (p-m m) 500 100 1400 100 (+ (p-cs m) (score scorelist)) (p-ms m) 30 5 0))
(define (mwins m) (make-p (p-m m) 500 100 1400 100 (p-cs m) (+ (p-ms m) (score scorelist)) 30 0 5))

(define (kh m key) (if (equal? (p-m m) "mouse") m
                       (cond [(key=? w) (make-package (make-p "cat" (p-cx m) (min 990 (+ (p-cy m) 10)) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm (p-cx m) (min 990 (+ (p-cy m) 10))))]
                             [(key=? a) (make-package (make-p "cat" (min 1500 (+ (p-cx m) 10)) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm(min 1500 (+ (p-cx m) 10)) (p-cy m)))]
                             [(key=? s) (make-package (make-p "cat" (p-cx m) (max 0 (- (p-cy m) 10)) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm (p-cx m) (max 0 (- (p-cy m) 10))))]
                             [(key=? d) (make-package (make-p "cat" (max 0 (- (p-cx m) 10)) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm (max 0 (- (p-cx m) 10)) (p-cy m)))]
                             [else m])))

(define (rh m msg)
  (cond [(string=? (p-m m) "cat") (make-p "cat" (p-cx m) (p-cy m) (posm-x msg) (posm-y msg) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m))]
        [else (make-p "mouse" (posm-x msg) (posm-y msg) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m))]))
