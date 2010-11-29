;;;;
;;;; 	Copyright (C) 2010 Neil Jerram.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA

(define-module (ossau primes)
  #:export (prime?
	    pi
	    make-goldbach))

(define (cached-vector-proc init length-increment fill calculator)
  (let ((v init))
    (lambda (n)
      (if (>= n (vector-length v))
	  (let* ((old-v v)
		 (old-nmax (1- (vector-length old-v)))
		 (nmax (max n (+ old-nmax length-increment))))
	    (set! v (make-vector (1+ nmax) fill))
	    (vector-move-left! old-v 0 (vector-length old-v) v 0)
	    (calculator v nmax old-nmax)))
      (vector-ref v n))))

;; Is N a prime number?
(define prime?
  (cached-vector-proc (vector #f #f)
		      100
		      #t
		      (lambda (v nmax old-nmax)
			(let loop ((i 2))
			  (or (> i nmax) ; finished
			      (begin
				(if (vector-ref v i)
				    ;; i is prime
				    (let loop2 ((j (* (max 2
							   (quotient
							    (+ old-nmax i)
							    i))
						      i)))
				      (or (> j nmax)
					  (begin
					    (vector-set! v j #f)
					    (loop2 (+ j i))))))
				(loop (1+ i))))))))

;; The number of primes <= N.
(define pi
  (cached-vector-proc (vector 0)
		      100
		      0
		      (lambda (v nmax old-nmax)
			(let loop ((i old-nmax)
				   (nprimes (vector-ref v old-nmax)))
			  (vector-set! v i nprimes)
			  (or (= i nmax)
			      (loop (1+ i)
				    (+ nprimes (if (prime? (1+ i)) 1 0))))))))

;; Construct a procedure that returns GP(n), for n in 0..nmax, where
;; GP(n) is the number of Goldbach partitions of 2n.
(define (make-goldbach nmax)
  (let ((v (make-vector (1+ nmax) 0)))
    (let loopi ((i 3))
      (or (> i nmax)
	  (begin
	    (if (prime? i)
		(let loopj ((j i))
		  (let ((n (/ (+ i j) 2)))
		    (or (> n nmax)
			(begin
			  (if (prime? j)
			      (begin
				;(format #t "~a = ~a + ~a\n" (* n 2) i j)
				(vector-set! v n (1+ (vector-ref v n)))))
			  (loopj (1+ j)))))))
	    (loopi (1+ i)))))
    ;; One special case: n=2 => 2n=4 is the only number with a
    ;; Goldbach pair involving 2 (i.e. 2+2).
    (or (zero? (vector-ref v 2)) (error "Expected v[2] to be 0"))
    ;(vector-set! v 2 1)
    ;; As the return value, construct a lambda that queries whether a
    ;; given element of this vector is #t.
    (lambda (n)
      (vector-ref v n))))
