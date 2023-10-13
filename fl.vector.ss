;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#!chezscheme

(library (fl.vector)
  (export make-fl.vector fl.vector-sum fl.vector-length fl.vector-ref fl.vector-set!
          make-fl*vector fl*vector-sum)
  (import (chezscheme))

(eval-when (compile) (generate-interrupt-trap #f))         

#| a Fl.vector is either
   a Pair: (Fixnum . Flonum)  [length . fill value]
        or (Flvector . Unused)
   or a Flvector  [fl.vector- procedures can be applied to flvector arguments] |#
   
(define (make-fl.vector len flval)       ;; Fixnum Flonum -> Fl.vector
  (cons len flval))
  
(define (fl.vector-sum flvec)            ;; Fl.vector -> Flonum
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (if (fixnum? cf)
        (* cf (cdr flvec))
        (fl*vector-sum cf)))
    (fl*vector-sum flvec)))

(define (fl.vector-set! flvec i flval)   ;; Fl.vector Fixnum Flonum ->
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (when (fixnum? cf)
        (set-car! flvec (make-fl*vector cf (cdr flvec))))
      (flvector-set! (car flvec) i flval))
    (flvector-set! flvec i flval)))

(define (fl.vector-length flvec)         ;; Fl.vector -> Fixnum
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (if (fixnum? cf)  cf
          (flvector-length cf)))
    (flvector-length flvec)))

(define (fl.vector-ref flvec i)          ;; Fl.vector Fixnum -> Flonum
  (if (pair? flvec)
    (let ([cf (car flvec)])
      (if (fixnum? cf)  (cdr flvec)
          (flvector-ref cf i)))
    (flvector-ref flvec i)))
      
(define (make-fl*vector len flval)       ;; Fixnum Flonum -> Flvector
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
