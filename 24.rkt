(require picturing-programs)
(require "posm.rkt")
(define (distance x1 y1 x2 y2) (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))



(define catimage (scale 0.2 (bitmap/url "https://raw.githubusercontent.com/MasonQiao/Ch-24-Universe-Project/main/cat.png")))
(define mouseimage (scale 0.2 (bitmap/url "https://raw.githubusercontent.com/MasonQiao/Ch-24-Universe-Project/main/mouse.png")))

(define floorimage (bitmap/url "https://raw.githubusercontent.com/MasonQiao/Ch-24-Universe-Project/main/floor.png"))


(define scorelist (list 1 2 5 10 20 50 100 200 500 1000 2000 5000 10000))
(define (aux dummy) (if (= (random 2) 1) true false))
(define (score l)
  (cond [(empty? l) 1000000000]
        [else (if (aux "random idiot") (first l) (score (rest l)))]))


(define-struct p (m cx cy mx my cs ms t ct mt))
;BASE MODEL: (make-p (p-m m) (p-cx m) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m))


(define (dh1 m) (overlay/align "middle" "bottom" (text (string-append "Cat: " (number->string (p-cs m)) " Mouse: " (number->string (p-ms m))) 20 "black") (dh m)))

(define (dh m) (cond
                 [(> (p-ct m) 0) (overlay/align "middle" "top" (text "Mouse was caught. Cat wins." 20 "black") (place-image mouseimage (p-mx m) (p-my m) (place-image catimage (p-cx m) (p-cy m) floorimage)))]
                 [(> (p-mt m) 0) (overlay/align "middle" "top" (text "Time ran out. Mouse wins." 20 "black") (place-image mouseimage (p-mx m) (p-my m) (place-image catimage (p-cx m) (p-cy m) floorimage)))]
                 [else (overlay/align "middle" "top" (text (string-append "Time Left: " (number->string-digits (p-t m) 2)) 20 "black") (place-image mouseimage (p-mx m) (p-my m) (place-image catimage (p-cx m) (p-cy m) floorimage)))]))

(define (th m) (if (<= (p-t m) 0) (mwins m) (if (< (distance (p-cx m) (p-cy m) (p-mx m) (p-my m)) 100) (cwins m) (make-p (p-m m) (p-cx m) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (max 0 (- (p-t m) 0.01)) (max 0 (- (p-ct m) 0.01)) (max 0 (- (p-mt m) 0.05))))))
(define (cwins m) (make-p (p-m m) 500 100 1400 100 (+ (p-cs m) (score scorelist)) (p-ms m) 20 5 0))
(define (mwins m) (make-p (p-m m) 500 100 1400 100 (p-cs m) (+ (p-ms m) (score scorelist)) 20 0 5))

(define (kh m key) (if (string=? (p-m m) "mouse") m
                       (cond [(key=? key "w") (make-package (make-p "cat" (p-cx m) (max 0 (- (p-cy m) 10)) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm "m" (p-cx m) (max 0 (- (p-cy m) 10))))]
                             [(key=? key "a") (make-package (make-p "cat" (max 0 (- (p-cx m) 10)) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm "m" (max 0 (- (p-cx m) 10)) (p-cy m)))]
                             [(key=? key "s") (make-package (make-p "cat" (p-cx m) (min 990 (+ (p-cy m) 10)) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm "m" (p-cx m) (min 1500 (+ (p-cy m) 10))))]
                             [(key=? key "d") (make-package (make-p "cat" (min 1500 (+ (p-cx m) 10)) (p-cy m) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm "m" (min 1500 (+ (p-cx m) 10)) (p-cy m)))]
                             [else m])))

(define (rh m msg)
  (cond [(and (string=? (posm-to msg) "c") (string=? (p-m m) "cat")) (make-p "cat" (p-cx m) (p-cy m) (posm-x msg) (posm-y msg) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m))]
        [(and (string=? (posm-to msg) "m") (string=? (p-m m) "mouse")) (make-p "mouse" (posm-x msg) (posm-y msg) (p-mx m) (p-my m) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m))]
        [else m]))

(define (bx a) (max 0 (min 1500 a)))
(define (by a) (max 0 (min 990 a)))

(define (mh m x y event) (if (string=? (p-m m) "cat") m
                (make-package (make-p "mouse" (p-cx m) (p-cy m) (bx (+ (* 0.99 (p-mx m)) (* 0.01 x))) (by (+ (* 0.99 (p-my m)) (* 0.01 y))) (p-cs m) (p-ms m) (p-t m) (p-ct m) (p-mt m)) (make-posm "c" (bx (+ (* 0.9 (p-mx m)) (* 0.1 x))) (by (+ (* 0.9 (p-my m)) (* 0.1 y)))))))



(define (start-bang title initial)
  (big-bang initial
    (name title)
    (register LOCALHOST) ;; see 08 Server
    (to-draw dh1)
    (on-key kh)
    (on-tick th 0.01)
    (on-receive rh)
    (on-mouse mh)))

(launch-many-worlds
 (start-bang "cat" (make-p "cat" 500 100 1400 100 0 0 20 0 0))
 (start-bang "mouse" (make-p "mouse" 500 100 1400 100 0 0 20 0 0)))
