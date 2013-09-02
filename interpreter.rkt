#lang racket

(require racket/match)

;; Small language interpreter by Quinn Maurmann for blog post "What is a programming language?"
;; qmaurmann.wordpress.com/



;; 0. DATA DEFINITIONS

;; Supported syntax (a subset of Racket):
;;   <expr> ::= <symbol>          -- the literal Racket type
;;            | <number>          -- ditto
;;            | <boolean>         -- ditto
;;            | (void <expr>*)    -- takes any number of arguments
;;            | (+ <expr> <expr>)
;;            | (* <expr> <expr>)
;;            | (< <expr> <expr>)
;;            | (= <expr> <expr>)
;;            | (if <expr> <expr> <expr>)
;;            | (lambda (<symbol>*) <expr>)
;;            | (begin <expr> <expr>*)
;;            | (set! <symbol> <expr>)
;;            | (let ([<symbol> <expr>]*) <expr>)
;;            | (letrec ([<symbol> <expr>]*) <expr>)
;;            | (<expr> <expr>*)

;; From the REPL only, one may also use top-level definitions
;;   <statement> ::= (repl-bind <symbol> <expr>)

;; Semantics (basically identical to Racket):
;;   <value>   ::= <number>
;;               | <boolean>
;;               | #<void>        -- the Racket singleton
;;               | <closure>
;;   <closure> ::= (closure (<symbol>*) <expr> <environment>)
;;                                -- i.e. a heterogeneous 4-element list, beginning with the
;;                                -- symbol closure, followed by a list of symbols

;; Data definitions for eval:
;;   <location>         ::= <integer>
;;   <name-binding>     ::= (<symbol> <location>)       -- i.e. a 2-element heterogeneous list
;;   <location-binding> ::= (<location> <value>)
;;   <environment>      ::= (<name-binding>*)           -- i.e. a list of name-bindings
;;   <store>            ::= ((<location-binding>*) <location>)

;; Invariant: the additional location carried by a store is to be the max of all
;; locations used in the list of location-bindings (so that fresh locations can easily
;; be found)

;; Possible future TODO: change environments and stores from association lists to AVL or
;; red-black trees for efficiency.



;; 1. USEFUL LIST DEFINITIONS

(define default-env '())
(define default-sto (list '() -1))

(define (extend-store loc val sto)
  (match sto
    [(list pairs m) (list (cons (list loc val) pairs) (max m loc))]
    [else (error "internal error: invalid store in extend-store")]))

(define (zip lst1 lst2)
  ;; zip a pair of lists to a list of pairs
  (map list lst1 lst2))

(define (unzip lst)
  ;; unzip a list of pairs into a pair of lists
  (match lst
    ['() '(() ())] ;; pair of empty lists
    [(cons (list fst snd) more-pairs)
     (match (unzip more-pairs)
       [(list lst1 lst2)
        (list (cons fst lst1) (cons snd lst2))])]
    [else (error "internal error: bad argument to unzip (not a list of pairs)")]))

(define (all predicate lst)
  (foldl (lambda (item result)
           (and result (predicate item)))
         #t
         lst))

(define (list-of-symbols? lst)
  (and (list? lst)
       (all symbol? lst)))

(define (list-of-bindings? lst)
  ;; a quick check for syntax in let/letrec expressions
  (and (list? lst)
       (all binding? lst)))

(define (binding? b)
  (match b
    [(list (? symbol? _) _) #t]
    [else #f]))



;; 2. EVAL AND ITS HELPERS

(define (eval expr env sto)
  (cond
    [(symbol? expr) (lookup-symbol expr env sto)]
    [(number? expr) (list expr sto)]
    [(boolean? expr) (list expr sto)]
    ;; else expr should be a quoted list
    [(list? expr)
     (match expr
       ['() (error "error: empty expression")]
       [(cons fst _)
        (match fst
          ['void (list (void) sto)]
          ;; primitive ops
          [(? (lambda (s) (member s '(+ * < =))) _)
           (eval-binary-primitive expr env sto)]
          ;; special forms
          ['if (eval-if expr env sto)]
          ['lambda (eval-lambda expr env sto)]
          ['begin (eval-begin expr env sto)]
          ['set! (eval-set! expr env sto)]
          ;; syntactic sugar
          ['let (eval-let expr env sto)]
          ['letrec (eval-letrec expr env sto)]
          ;; finally, application:
          [else (eval-apply expr env sto)])])]
    ;; something else?
    [else (error "error: expressions must be symbols, numbers, booleans, or lists of expressions:"
                 expr)]))


(define (lookup-symbol s env sto)
  (match (assoc s env)
    [(list _ loc)
     (match (assoc loc (first sto))
       [(list _ val) (list val sto)]
       [else (error "internal error: bad store location in lookup-symbol")])]
    [else (error "error: unbound identifier:" s)]))


(define (eval-sequence exprs env sto)
  ;; evaluate a list of exprs in sequence, passing the store from one step to the next,
  ;; returning 2-element list of the form (list list-of-evaluated-exprs new-store)
  (define (eval-one-and-cons expr ptl-result)
    (match ptl-result
      [(list values sto1)
       (match (eval expr env sto1)
         [(list val sto2) (list (cons val values) sto2)]
         [else (error "internal error: bad return value in eval in eval-sequence")])]
      [else (error "internal error: bad argument to eval-one-and-cons in eval-sequence")]))
  ;; now foldl and reverse it
  (match (foldl eval-one-and-cons (list '() sto) exprs)
    [(list backwards-values store) (list (reverse backwards-values) store)]
    [else (error "internal error: bad return value in eval-sequence")]))

(define (eval-binary-primitive expr env sto)
  (match expr
    [(list sym left right)
     (match (eval-sequence (list left right) env sto)
       [(list (list l r) sto2)
        (if (and (number? l) (number? r))
            (let ([op (match sym
                        ['+ +] ['* *] ['< <] ['= =]
                        [else (error (string-append "internal error: not a binary primitive "
                                                    (symbol->string sym)))])])
              (list (op l r) sto2))
            (error (string-append "error: non-numeric argument to primitive " sym)))]
       [else (error "internal error: bad return value in eval-sequence in eval-binary-primitive")])]
    [else (error "error: invalid binary primitive expression:" expr)]))


;; special forms:

(define (eval-if expr env sto)
  (match expr
    [(list 'if test conseq alt)
     (match (eval test env sto)
       [(list #f sto1) (eval alt env sto1)]
       ;; Racket treats any non-false value as true:
       [(list _ sto1) (eval conseq env sto1)]
       [else (error "internal error: bad return value in eval in eval-if")])]
    [else (error "error: invalid if expression:" expr)]))

(define (eval-lambda expr env sto)
  (match expr
    [(list 'lambda (? list-of-symbols? params) body)
     (list (list 'closure params body env) sto)]
    [else (error "error: invalid lambda expression:" expr)]))

(define (eval-begin expr env sto)
  (match expr
    [(cons 'begin (cons fst rst))
     (match (eval-sequence (cons fst rst) env sto)
       [(list values new-sto) (list (last values) new-sto)]
       [else (error "internal error: bad return value in eval-sequence in eval-begin")])]
    [else (error "error: invalid begin expression:" expr)]))

(define (eval-set! expr env sto)
  (match expr
    [(list 'set! (? symbol? var) expr1)
     (match (eval expr1 env sto)
       [(list val sto1)
        (match (assoc var env)
          [(list _ loc) (list (void) (extend-store loc val sto1))]
          [else (error "error: unbound identifier in set!:" expr)])]
       [else (error "internal error: problem with return type in eval in eval-set!")])]
    [else (error "error: invalid set! expression:" expr)]))


;; syntactic sugar:

(define (eval-let expr env sto)
  (match expr
    [(list 'let (? list-of-bindings? bindings) body)
     (match (unzip bindings)
       [(list names exprs)
        ;; desugar into lambda and call eval-apply:
        (eval-apply (cons (list 'lambda names body) exprs) env sto)]
       [else (error "internal error: problem unzipping bindings in let expression")])]
    [else (error "error: invalid let expression:" expr)]))

(define (eval-letrec expr env sto)
  (match expr
    [(list 'letrec (? list-of-bindings? bindings) body)
     (match (unzip bindings)
       [(list names exprs)
        ;; desugar into let and set! and call eval-let:
        (eval-let (list 'let
                        (for/list ([name names])
                          (list name '(void)))
                        (append (cons 'begin (for/list ([pair bindings])
                                               (cons 'set! pair)))
                                (list body)))
                  env sto)]
       [else (error "internal error: problem unzipping bindings in letrec expression")])]
    [else (error "error: invalid letrec expression:" expr)]))


;; application:

(define (eval-apply expr env sto)
  (match expr
    [(cons proc args)
     (match (eval proc env sto)
       [(list proc-val sto1)
        (match proc-val
          [(list 'closure proc-params _ _)
           (let ([n (length proc-params)])
             (if (eq? n (length args))
                 (match (eval-sequence args env sto1)
                   [(list arg-vals sto2)
                    (apply-closure proc-val arg-vals sto2)]
                   [else (error "internal error: problem with eval-sequence in eval-apply")])
                 (error "error: arity mismatch to compound procedure:" expr)))]
          [else (error "error: not a procedure to apply in:" expr)])]
       [else (error "internal error: problem with eval in eval-apply")])]
    [else (error "error: invalid application expression:" expr)]))
     
(define (apply-closure proc-val arg-vals sto)
  (match proc-val
    [(list 'closure proc-params proc-body proc-env)
     (match sto
       [(list sto-pairs sto-max)
        (let* ([n (length proc-params)]
               [new-locs (range (+ 1 sto-max) (+ 1 n sto-max))]
               [name-bindings (zip proc-params new-locs)]
               [loc-bindings (zip new-locs arg-vals)]
               [new-env (append name-bindings proc-env)]
               [new-sto (list (append loc-bindings sto-pairs) (+ n sto-max))])
          (eval proc-body new-env new-sto))]
       [else (error "internal error: problem with store in apply-closure")])]
    [else (error "internal error: not a closure given to apply-closure")]))



;; 3. SELECTED TESTS


(define set!-test                       ;; correct value: 5
  '(let ([x 2] [y 3])
     (begin
       (set! x (begin
                 (set! y 5)
                 y))
       x)))

(define fact-test                       ;; correct value: 9332...0000,
  '(letrec ([fact (lambda (n)           ;; a 158-digit number
                    (if (= n 0)
                        1
                        (* n (fact (+ n -1)))))])
     (fact 100)))

(define acc-test                        ;; correct value: 222
  '(letrec ([make-accumulator
             (lambda (n)
               (lambda (x)
                 (begin (set! n (+ n x))
                        n)))])
     (begin (letrec ([a (make-accumulator 100)])    ;; example usage of
              (begin                                ;; make-accumulator
                (a 12)
                (a 10)
                (letrec ([b (make-accumulator 40)])
                  (begin
                    (b 5)
                    (a 100))))))))

;; For testing: Uncomment the (map run ...) line below to test above programs before launching repl
(define (run expr)
  (first (eval expr default-env default-sto)))

;; (map run (list set!-test fact-test acc-test))



;; 4. REPL

(define (show value)
  (match value
    [(? number? _) (displayln value)]
    [(? boolean? _) (displayln value)]
    [(? void? _) (void)]
    [(cons 'closure _) (displayln "#<procedure>")]
    [else (error "internal error: bad value to show")]))

;; Inelegant, and possibly repeating myself (I'm basically re-implementing a more
;; complex letrec by hand, although only in one variable). The trickiness here is due
;; to passing *environments* between function calls, rather than just stores.

(define (repl env sto)
  (let ([expr (read)])
    (match expr
      [(cons 'repl-bind more)
       (match more
         [(list (? symbol? s) expr1)
          (match sto
            [(list pairs mx)
             (let* ([new-loc (+ 1 mx)]                              ;; create "empty"
                    [env1 (cons (list s new-loc) env)]              ;; location before
                    [sto1 (list (cons (list new-loc (void)) pairs)  ;; evalutating expr1
                                (+ 1 new-loc))])
               (match (eval expr1 env1 sto1)                        ;; then eval
                 [(list value (list sto2-pairs sto2-mx))
                  (repl env1 (list (cons (list new-loc value) sto2-pairs)  ;; then rebind
                                   sto2-mx))]
                 [else (error "internal error: bad return in eval in repl")]))]
            [else (error "internal error: bad store returned in repl")])]
         [else (error "error: bad repl-bind statement:" expr)])]
      [else (match (eval expr env sto)
              [(list val new-sto)
               (show val)
               (repl env new-sto)]
              [else (error "internal error: bad return in eval in repl")])])))

(repl default-env default-sto)  ;; start it!