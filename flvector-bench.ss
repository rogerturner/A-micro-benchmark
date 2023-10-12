#! /usr/local/bin/scheme --optimize-level 3 --program
;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#!chezscheme

(import (chezscheme) (fl.vector))

(define (flvector-sum flvec)
  (let loop ([len (flvector-length flvec)] [i 0] [sum 0.0])
    (if (fx<? i len)
      (loop len (fx1+ i) (fl+ sum (flvector-ref flvec i)))
      (fl+ sum))))

(define (flvector-bench make update sum len flval)
  (let ([flvec (make len flval)])
    (update flvec 0 flval)
    (sum flvec)))

(define (repeat make update sum description)
  (display description)
  (display (flvector-bench make update sum 100000 0.5)) (newline)
  (time
    (do ([i 0 (fx+ i 1)])
        ((fx= i 1000))
      (flvector-bench make update sum 100000 0.5))))

(define (run)
  (repeat make-flvector  flvector-set!  flvector-sum  "baseline ")
  (repeat make-fl*vector flvector-set!  flvector-sum  "unroll make ")
  (repeat make-flvector  flvector-set!  fl*vector-sum "unroll sum ")
  (repeat make-fl*vector flvector-set!  fl*vector-sum "unroll both ")
  (repeat make-fl.vector fl.vector-set! fl.vector-sum "fl. struct+unroll ")
  (repeat make-fl.vector (lambda _ #f)  fl.vector-sum "fl. \"lazy\" :-) "))

(run)  ;; (to run without "whole program" compilation)

;(compile/run (command-line-arguments))

(define (compile/run args)               ;; ListOfString ->
  ;; whole program compile with inlining, no debug, etc; run
  (if (and (pair? args) (string=? "run" (car args)))
    (run)
  (parameterize (
      (generate-wpo-files #t)
      (compile-imported-libraries #t)
      (enable-cross-library-optimization #t)
      (optimize-level 3)
      (debug-level 0)
      (commonization-level 0)
      (generate-allocation-counts #f)
      (undefined-variable-warnings #t)
      (compile-file-message #f)
      ;(following settings don't improve this micro-benchmark)
      ;(generate-inspector-information #f)
      ;(generate-procedure-source-information #f)
      ;(generate-interrupt-trap #f)        ;; (only appropriate for library code)
      ;(#%$optimize-closures #t)
      ;(#%$track-dynamic-closure-counts #f)
      ;(cp0-effort-limit 20000)            ;; (default is 200)
      ;(cp0-score-limit   2000)            ;; (default is 20)
      ;(cp0-outer-unroll-limit 0)          ;; (default is 0) [1 slower]
      )
    (compile-library "flvector")
    (compile-program "f64vec-bench")
    (let ([missing
        (compile-whole-program "f64vec-bench.wpo" "f64vec-bench.wp")])
      (if (null? missing)
        (let ([command (string-append "scheme --program f64vec-bench.wp run ")])
          (system command)
          (exit))
        (display missing))))))

#| Sample output:
baseline 50000.0
(time (do ((...)) ...))
    91 collections
    0.194259542s elapsed cpu time, including 0.002983000s collecting
    0.194262000s elapsed real time, including 0.003029000s collecting
    800091824 bytes allocated, including 799743872 bytes reclaimed
unroll make 50000.0
(time (do ((...)) ...))
    91 collections
    0.144893750s elapsed cpu time, including 0.002267000s collecting
    0.144889000s elapsed real time, including 0.002314000s collecting
    800091696 bytes allocated, including 798505728 bytes reclaimed
unroll sum 50000.0
(time (do ((...)) ...))
    91 collections
    0.120695207s elapsed cpu time, including 0.002321000s collecting
    0.120693000s elapsed real time, including 0.002361000s collecting
    800091696 bytes allocated, including 801707520 bytes reclaimed
unroll both 50000.0
(time (do ((...)) ...))
    91 collections
    0.075367584s elapsed cpu time, including 0.002303000s collecting
    0.075367000s elapsed real time, including 0.002339000s collecting
    800091696 bytes allocated, including 801707520 bytes reclaimed
fl. data struct 50000.0
(time (do ((...)) ...))
    91 collections
    0.075227834s elapsed cpu time, including 0.002293000s collecting
    0.075224000s elapsed real time, including 0.002343000s collecting
    800107696 bytes allocated, including 801723424 bytes reclaimed
:-) 50000.0
(time (do ((...)) ...))
    no collections
    0.000007041s elapsed cpu time
    0.000007000s elapsed real time
    32000 bytes allocated
|#

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
