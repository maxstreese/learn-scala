# Redbook (Functional Programming In Scala) Exercises And Notes

Version: @VERSION@

## Chapter 01

- Benefit of functional programming: Modularity. Pure functions are more modular than non-pure functions
  - More modular means: Easier to test, reuse, parallelize, generalize (what does that mean?) and reason about
- Referential Transparency
  - Definition
    - An expression e is referentially transparent if, for all programs p, all occurrences of e in p can be replaced by
      the result of evaluating e without affecting the meaning of p.
    - A function f is pure if the expression f(x) is referentially transparent for all referentially transparent x.
  - Alternative ways of looking at it
    - A referentially tranparent expression does not depend on context and therefore can be reasoned about locally.
      An example given in the book that makes this clear is exceptions which are not referentially transparent because
      they affect the programm differently depending on whether they are caught or not

## Chapter 03

- Regarding exercise 3.7: I do not think that `foldRight` as implemented can halt the recursion and short circuit.
  This is due to the fact that such halting is not required in the structure of the fold which you cannot simply
  escape from within

## Chapter 05

- Strictness
  - Definition
    - If the evaluation of an expression runs forever or throws an error instead of returning a definite value, we say
      that the expression does not terminate, or that it evaluates to bottom. A function f is strict if the expression
      f(x) evaluates to bottom for all x that evaluate to bottom
