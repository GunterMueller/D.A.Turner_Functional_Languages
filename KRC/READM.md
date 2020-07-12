 KRC- Kent Recursive Calculato
 From "http://www.krc-lang.org/"
Kent Recursive Calculator

KRC is a simple lazy functional language designed and implemented by David Turner around 1979-81. It was a "miniaturised" version of SASL (St Andrews Static Language), used for teaching functional programming at the University of Kent and Oxford University in the early 1980's.

The software has been brought back to life by translating the original BCPL sources to C and adapted to run under Unix. (Warning: it doesn't work with gcc 4.9, you need an earlier gcc, or another C compiler such as clang or tcc.)

Download sources of Unix KRC, this is free software, see "COPYING" for license.

The Unix manual page krc.1

The KRC prelude of standard definitions, this is a new prelude written for the Unix version.

See also the 1981 prelude. Comments have been added to explain definitions which might otherwise be obscure. Reference (1) below as originally published included this, or most of it, as Appendix.

Send bug reports and, if known, bug fixes to krc@krc-lang.org
Papers
These papers are included by permission of Cambridge University Press and ACM respectively, see notices at the foot of the title page in each case. They may be freely copied but commercial publication rights remain with the publishers, who are the copyright holders.

    D. A. Turner Recursion Equations as a Programming Language, in Functional Programming and its Applications, pp1-28, Cambridge University Press, 1982 (eds. Darlington, Henderson and Turner).

    This paper contains an overview of KRC (starting on page 6) with examples including the first published account of the list-of-successes method for avoiding backtracking, here applied to the eight queens problem.
    D. A. Turner The Semantic Elegance of Applicative Languages, Proceedings MIT/ACM conference on Functional Languages and Architectures, pp 85--92, Portsmouth, New Hampshire, October 1981.

    The paper tackles a programming problem in KRC: enumerating paraffin molecules. It is somewhat misnamed - I was given this title by the program committee when they invited me to give a talk and I omitted to change it. A more appropriate title for the paper would have been Programming with Set Abstraction.

Software Archeology
For historical interest here are sources of EMAS KRC. Not now usable, due to antique BCPL syntax and interface to an extinct operating system, it ran on the University of Kent's ICL 2900 computer under EMAS, during the period 1980-6. There was a second implementation of KRC made around 1985 by Simon Croft, in C for Unix using combinators, which remained in use for longer, but we have been unable to locate Simon, or the code. 
