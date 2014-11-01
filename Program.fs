// Scheme interpreter in F#
// ref: http://norvig.com/lispy2.html

module FLispy.Program

open System
open System.Collections.Generic
open System.Globalization
open System.Reflection

open FLispy.Runtime
open FLispy.Lexer

let program_env =
    let bindings =
        Map.ofList [
            lookup_symbol("foo"), ref (List([Number(1); Number(2); Number(3);]))
        ]

    appendFrame bindings global_env

let eval_string (str: string) (env: Environment) =
    eval (parse_string str) env |> to_str

eval_string "(+ 40 2)" program_env |> Console.WriteLine

let assert' exp str =
    let res = eval_string str program_env
    if exp <> res then
        sprintf "Expected %s got %s" exp res |> Console.WriteLine

assert' "3"              "(length foo)"
assert' "1"              "(car foo)"
assert' "(2 3)"          "(cdr foo)"
assert' "#f"             "(null? foo)"
assert' "(1 2 3)"        "'(1 2 3)"
assert' "4"              "(length '(1 2 3 4))"
assert' "1"              "(if #t 1 0)"
assert' "0"              "(if #f 1 0)"
assert' "1"              "(if #t 1)"
assert' "#t"             "(list? '(1 2))"
assert' "#f"             "(list? 1)"
assert' "42"             "(+ 10 20 12)"
assert' "-4"             "(- 1 2 3)"
assert' "(1 2 3)"        "(list 1 2 3)"
assert' "(list 1 2 3)"   "'(list 1 2 3)"
assert' "12"             "(begin (define x 12) x)"
assert' "42"             "(begin (define answer 30) (set! answer (+ answer 12))  answer)"
assert' "9"              "(begin (define square (lambda (x) (* x x))) (square 3))"
assert' "233"            "
(begin
  (define fib
    (lambda (n)
      (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))
  (fib 13))"

assert' "64"             "(square 8)" 

assert' "80"             "
(begin
  (define make-account
    (lambda (balance)
      (lambda (amt) 
        (begin
          (set! balance (+ balance amt))
          balance))))

  (define account (make-account 100)) 
  (account -20))"

assert' "()"             "(define twice (lambda (x) (* 2 x)))"
assert' "()"             "(define compose (lambda (f g) (lambda (x) (f (g x)))))"
assert' "(10)"           "((compose list twice) 5)"
assert' "()"             "(define repeat (lambda (f) (compose f f)))"
assert' "80"             "((repeat (repeat twice)) 5)"
assert' "()"             "(define abs (lambda (n) ((if (> n 0) + -) 0 n)))"
assert' "(3 0 3)"        "(list (abs -3) (abs 0) (abs 3))"

assert' "()"             "
(define combine (lambda (f)
    (lambda (x y)
      (if (null? x) (quote ())
          (f (list (car x) (car y))
             ((combine f) (cdr x) (cdr y)))))))"

assert' "()"             "(define zip (combine cons))"

assert' "((1 5) (2 6) (3 7) (4 8))"  "(zip (list 1 2 3 4) (list 5 6 7 8))"

assert' "()"             "
(define riff-shuffle
  (lambda (deck)
    (begin
      (define take
        (lambda (n seq)
          (if (<= n 0)
            (quote ())
            (cons (car seq) (take (- n 1) (cdr seq))))))
      (define drop
        (lambda (n seq)
          (if (<= n 0)
            seq
            (drop (- n 1) (cdr seq)))))
      (define mid
        (lambda (seq)
          (/ (length seq) 2)))
      ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))"

assert' "(1 5 2 6 3 7 4 8)"          "(riff-shuffle (list 1 2 3 4 5 6 7 8))"
assert' "(1 3 5 7 2 4 6 8)"          "((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))"
assert' "(1 2 3 4 5 6 7 8)"          "(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))"

assert' "()"                         "(define (add x y) (+ x y))"
assert' "42"                         "(add 40 2)"

assert' "42"                         "(eval '(* 21 2))"
