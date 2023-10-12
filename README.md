<!-- SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com> -->

### Micro-Benchmark of Summing a Floating Point Vector

ChezScheme; inspired by [this Racket Discourse thread](https://racket.discourse.group/t/a-micro-benchmark/2347)

tl;dr:

    *Version*         *Timings in seconds for 1000 executions*
    baseline           .19
    unroll make        .14
    unroll sum         .12
    unroll both        .07
    fl. struct+unroll  .07
    fl. "lazy"         .000007  :-)


### License

LGPL-3.0-or-later; © 2023 Roger Turner <r@rogerturner.com>