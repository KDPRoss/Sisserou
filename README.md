# Sisserou

Sisserou is a 'conceptual linguistic toy'. It in *no* way
intends to be useful per se: It is 'extremely pure', and
there are no facilities to interact with the 'external
world'.

## Goals

It is designed with several straightforward design goals
centring round conceptual and implementation simplicity:

- absolutely pure -- other than time (which is assumed to be
  unobservable), there are no 'real' effects

- conceptually-compact uniform syntax -- few constructs, easy
  to parse

- simple type implementation -- entirely explicit, no
  unification

- simple evaluator -- it's only 31 lines of code

- minimal REPL -- supports only the essential requisite
  functionality to interact with the toy (the only
  exception is the ability to read files, which makes
  interacting much more pleasant)

As a proof of concept, the core functionality -- the type
system and evaluator -- of Sisserou have been implemented
*in Sisserou*; this is presented along with several
(very-much-incomplete) test cases.

## Linguistic Heritage

Sisserou draws it's type- and language-theoretic basis from
System F Omega (<http://en.wikipedia.org/wiki/System_F>)
plus pattern matching a la ML. The language's syntax (i.e.,
the uniform abstraction syntax) is loosely based on that of
the Coquand and Huet's 1985 presentation of the Calculus of
Constructions ('Constructions: A Higher-Order Proof System
for Mechanising Mathematics') [[ Apparently, *this* notation
was based on Automath's. ]] -- although Sisserou's goals are
somewhat-more-modest than CoC's -- see that paper for a
defence of this syntax.

## What's Included

Here is the 'site map' of the implementation:

- Haskell code:

  - Sisserou.hs -- syntax representation, utilities, type
                   system

  - Parser.hs -- Hutton-Meijor-combinators-based parser

  - Eval.hs -- evaluator

  - Main.hs -- REPL, file loader

- Sisserou code:

  - Syntax.sro -- description and demonstration of syntactic
                  constructs and conventions

  - Test.sro -- some basic code defining 'standard' types
                (natural numbers, lists, maybe's, pairs),
                combinators related to these, and monads

  - Sisserou.sro -- Sisserou-in-Sisserou implementation;
                    intended to be a decent-sized body of
                    code (~1100 lines of code) demonstrating
                    language capabilities; it was also used
                    to debug the Haskell implementation

## How to Run It

(You will need the Haskell Stack (The `haskell-stack`
package in any fine GNU/Linux distro's package manager.)
installed.)

Running the interpreter:

- run `make`
