;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#!chezscheme

(library (fl.vector-with-examples)
  (export make-fl.vector fl.vector-sum fl.vector-length fl.vector-ref fl.vector-set!
          make-fl*vector fl*vector-sum)
  (import (chezscheme) (check-examples))

#| a Fl.vector is either
   a Pair: (Fixnum . Flonum)  [length . fill value]
        or (Flvector . Unused)
   or a Flvector  [fl.vector- procedures can be applied to flvector arguments] |#
   
(define (make-fl.vector len flval)       ;; Fixnum Flonum -> Fl.vector
  (example: (make-fl.vector 2 2.0) => '(2 . 2.0) )
  (cons len flval))

(define FLV22  (make-flvector  2 2.0))
(define FLV.22 (make-fl.vector 2 2.0))

(define (fl.vector-sum flvec)            ;; Fl.vector -> Flonum
  (example: (fl.vector-sum FLV22)  => 4.0 )
  (example: (fl.vector-sum FLV.22) => 4.0 )
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (if (fixnum? cf)
        (* cf (cdr flvec))
        (fl*vector-sum cf)))
    (fl*vector-sum flvec)))

(define (fl.vector-length flvec)         ;; Fl.vector -> Fixnum
  (example: (fl.vector-length FLV22)  => 2 )
  (example: (fl.vector-length FLV.22) => 2 )
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (if (fixnum? cf)  cf
          (flvector-length cf)))
    (flvector-length flvec)))

(define (fl.vector-ref flvec i)          ;; Fl.vector Fixnum -> Flonum
  (example: (fl.vector-ref FLV22 0)  => 2.0 )
  (example: (fl.vector-ref FLV.22 0) => 2.0 )
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (if (fixnum? cf)  (cdr flvec)
          (flvector-ref cf i)))
    (flvector-ref flvec i)))
      
(define (fl.vector-set! flvec i flval)   ;; Fl.vector Fixnum Flonum ->
  (example: (let ([FLV.22 FLV.22])
              (fl.vector-set! FLV.22 0 1.0)
              FLV.22) => '(#vfl(1.0 2.0) . 2.0) )
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (when (fixnum? cf)
        (set-car! flvec (make-fl*vector cf (cdr flvec))))
      (flvector-set! (car flvec) i flval))
    (flvector-set! flvec i flval)))

(define (make-fl*vector len flval)       ;; Fixnum Flonum -> Flvector
  (example: (make-fl*vector 0 #f) => #vfl() )
  (example: (make-fl*vector 1 1.) => #vfl(1.) )
  (example: (make-fl*vector 4 4.) => #vfl(4. 4. 4. 4.) )
  (example: (make-fl*vector 5 5.) => #vfl(5. 5. 5. 5. 5.) )
  (let ([flvec (make-flvector len)]
        [flval (fl+ flval)]
        [start (fxmod len 4)])
    (do ([i 0 (fx1+ i)])
        ((fx=? i start))
      (flvector-set! flvec i flval))
    (let loop ([i start])
      (if (fx<? i len)
        (begin
          (flvector-set! flvec i         flval)
          (flvector-set! flvec (fx+ i 1) flval)
          (flvector-set! flvec (fx+ i 2) flval)
          (flvector-set! flvec (fx+ i 3) flval)
          (loop (fx+ i 4)))
        flvec))))

(define (fl*vector-sum flvec)            ;; Flvector -> Flonum
  (example: (fl*vector-sum #vfl()) => 0.0 )
  (example: (fl*vector-sum FLV22)  => 4.0 )
  (example: (fl*vector-sum (make-flvector 5 5.0)) => 25.0 )
  (let* ( [len   (flvector-length flvec)]
          [start (fxmod len 4)])
    (let loop ([i    start]
               [sum0 (if (fx<? start 1)  0.0  (flvector-ref flvec 0))]
               [sum1 (if (fx<? start 2)  0.0  (flvector-ref flvec 1))]
               [sum2 (if (fx<? start 3)  0.0  (flvector-ref flvec 2))]
               [sum3 0.0])
      (if (fx<? i len)
        (loop (fx+ i 4)
              (fl+ sum0 (flvector-ref flvec i))
              (fl+ sum1 (flvector-ref flvec (fx+ i 1)))
              (fl+ sum2 (flvector-ref flvec (fx+ i 2)))
              (fl+ sum3 (flvector-ref flvec (fx+ i 3))))
        (fl+ sum0 sum1 sum2 sum3)))))

(check-examples)

(for-each display `(  ;; get n examples and clear *examples*
    ,(let-syntax ([n (lambda _ 
                       (let ([n (length *examples*)])
                         (set-examples (list))
                         n) )]) n)
    " examples checked\n"))

)

#| *Notices*

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  License: <https://spdx.org/licenses/LGPL-3.0-or-later.html>
  Contact: <https://github.com/rogerturner/contact/issues/new/choose>  |#
