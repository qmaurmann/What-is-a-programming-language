Proglang
========

This repository contains additional code for the blog post "What is a programming language?" at qmaurmann.wordpress.com

The file interpreter.rkt is an interpreter for a subset of the Racket programming language. Features supported include first-class functions, lexical variables, and recursion. For additional challenge, the interpreter is written in purely functional style, e.g. implementing the set! keyword without using set! or the like in the underlying Racket.

A program will consist of a single expression, i.e. one of the following:

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

Running interpreter.rkt in DrRacket launches a REPL, from which one can evaluate expressions. Several example programs are included in the file; these can by copy-pasted (without the leading quote ') into the REPL.

But the real value of a REPL is in the interactivity it provides. Thus the REPL also supports recursive, top-level bindings via *statements* of the form

    (repl-bind <symbol> <expr>)

Such forms are similar, but not identical, to define statements in Racket's REPL (which are in turn similar, but not identical to define statements in Racket *files*).

For instance, one can define the factorial function by entering

    (repl-bind fact
      (lambda (n)
        (if (= n 0)
            1
            (* n (fact (+ n -1))))))

into the REPL, and henceforth expressions like (fact 100) will be evaluated in the context of this binding.

*Mutually* recursive definitions are *not* natively supported via repl-bind (whereas they *are* supported in Racket's REPL via define). They do work just fine within a letrec, e.g. evalutation of

    (letrec ([even? (lambda (n)
                      (if (= n 0) #t (odd? (+ n -1))))]
             [odd? (lambda (n)
                     (if (= n 0) #f (even? (+ n -1))))])
      (even? 100))

returns true as expected. And yet all is not lost for repl-bind, because one can still *simulate* mutually recursive definitions in the REPL, e.g. by entering in sequence:

    (repl-bind odd? (void))
    
    (repl-bind even?
      (lambda (n) (if (= n 0) #t (odd? (+ n -1)))))
    
    (set! odd? (lambda (n) (if (= n 0) #f (even? (+ n -1)))))

so that even? and odd? become closed over one another.

On the other hand, there are cases which I think my REPL gets "right", where Racket's REPL "fails". I feel pretty strongly that

    (repl-bind x 10)
    (repl-bind f (lambda (y) (+ x y)))
    (repl-bind x 20)
    (f x)

is correct in giving 30 rather than 40. Haskell's GHCi REPL (with keyword let) agrees with my intuition, whereas Racket's REPL (with define) does not.

It's worth pointing out that Haskell has no analogue of set!, but that the GHCi let keyword can take simultaneous bindings (separated by semicolons), natively supporting mutually recursive definitions this way.

In any case, the top-level bindings in the REPL are there for convenience; I don't consider them *really* part of the language. If I were to support mutual recursion via repl-bind, it would be in the style of GHCi (as repl-bind is currently implemented analogous to a one-binding letrec).

Beyond the particulars of repl-bind, there are a few small differences in the semantics of the language herein and of Racket. Two known examples:

* The letrec form temporarily binds variables to

        #<void>
rather than

        #<undefined>
As in Racket, this value can be leaked by an expression like

        (letrec ([x x]) x)


* You can't easily shadow the keywords and primitive operations. For instance,

        (let ([+ (lambda (x y) (* x y))]) (+ 5 5))
evaluates to 10, not 25 as it would in Racket. This is because, while + *is* a valid symbol (and hence expression), the form

        (+ <expr> <expr>)
is is found before procedure application

        (<expr> <expr>*)
and so the value of + is never looked up. If one *must* shadow the builtins, there's always the option to write things like

        (let ([apply2 (lambda (f x y) (f x y))]
              [+ (lambda (x y) (* x y))])
          (apply2 + 5 5))
which indeed evaluates to 25.

