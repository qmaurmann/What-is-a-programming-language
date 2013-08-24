What-is-a-programming-language
==============================

This repository contains additional code for the blog post of same title at qmaurmann.wordpress.com

The file interpreter.rkt is an interpreter for the following subset of the Racket programming language. A program is a single expression, i.e. one of the following:

    <expr> ::= <symbol>               -- the literal Racket type
             | <number>               -- ditto
             | <boolean>              -- ditto
             | (void <expr>*)         -- takes any number of arguments
             | (+ <expr> <expr>)
             | (* <expr> <expr>)
             | (< <expr> <expr>)
             | (= <expr> <expr>)
             | (if <expr> <expr> <expr>)
             | (lambda (<symbol>*) <expr>)
             | (begin <expr> <expr>*)
             | (set! <symbol> <expr>)
             | (let ([<symbol> <expr>]*) <expr>)
             | (letrec ([<symbol> <expr>]*) <expr>)
             | (<expr> <expr>*)       -- procedure application

One runs a program by quoting it (in Racket) and passing it to the function run (or to the function eval with additional parameters default-env and default-sto). Several quoted test programs are included at the end of the file.

The semantics are intended to be almost identical to Racket. I am aware of a few small differences:

* The letrec form temporarily binds variables to #<void> rather than #<undefined>. As in Racket, this value can be leaked by an expression like
        (letrec ([x x]) x)

* You can't easily shadow the keywords and primitive operations. For instance,
        (let ([+ (lambda (x y) (* x y))]) (+ 5 5))
evaluates 10, not 25 as it would in Racket. This is because, while + *is* a valid symbol, the form
        (+ <expr> <expr>)
is is found before procedure application
        (<expr> <expr>*)
and so the value of + is never looked up. If one *must* shadow the builtins, there's always the option to write things like
        (let ([apply2 (lambda (f x y) (f x y))]
              [+ (lambda (x y) (* x y))])
          (apply2 + 5 5))
which does evaluate to 25

