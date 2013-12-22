fresh
=====

Fresh Programming Language

a lisp intepreter written in python
(define fibr
    (lambda [count current old]
        (if (< count 2)
            current
            (fibr (- count 1) (+ current old) current)
        )
    )
)
(define fib
    (lambda [index]
        (fibr index 1 0)
    )
)
(fib 4)
5

Features
========

Supports Booleans
-----------------
- true and false
- (not true) false - negate given boolean
- (and false true true) false - "and" any number of booleans together
- (or false true true) true - "or" any number of booleans together


Supports Integers
-----------------
- (+ 4 3 3) 10 - add any number of ints
- (- 5 1 1) 3 - subtract any number of ints from the first int
- (\* 2 2 2) 8 - multiply any number of ints
- (= 2 2 3) false - return true if any number of ints are equal
- (> 5 4 3) true - return true if any number of ints are in descending order
- (< 4 3 4) false - return true if any number of ints are in ascending order


Supports Conditional Branching
------------------------------
- (if true 4 0) 4 - branch based on condition



Supports variable assignment
----------------------------
- (define a 4)(+ a 1) 5 - define a mapping in the environment for later reference


Supports lambdas
----------------
- (lambda \[num\] (+ num 1)) - returns a lambda that will add one to it's single argument when called
- (define square (lambda \[i\] (\* i i)))(square 5) 25 - assign a lambda to a variable to reference later


Supports structs and member access
----------------------------------
- (struct x y) - returns a struct node that has members "x" and "y"
- (define person (struct height weight))(define quinten (person 178 77))quinten - returns a struct instantiation
    of a "person" with members "height" and "weight"
- (define person (struct height weight))(define quinten (person 178 77))(member quinten height)- returns the
    "person" "quinten"s "height" which is 178


Todo
====

- Implement type checker
- Add methods to structs to support classes
