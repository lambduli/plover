# Plover

Plover is a toy *automated theorem prover* based on my other small project [Minolog](https://github.com/lambduli/minilog).

The project explores a simple idea—a language like Prolog could be considered a theorem prover, we just need to use a *complete search strategy*.

Plover's syntax is a subset of Prolog's. This means it's limited to *Horn Clauses*.

The search strategy is very close to *Breadth-first Search* but not necessarily exactly it.

In ever other aspect it works exactly like the *Minilog*—it **uses unification** and is **not based** on *resolution*.

For more information about the abstract machine that both *Plover* and *Minilog* implement you can read the [write-up](https://github.com/lambduli/minilog/blob/main/WRITEUP.md) for *Minilog*.


## Usage

build: `cabal build`

run: `cabal run`

### REPL

In the REPL type `:load natural.pl` to load the definitions from the file.
You can then run the following query `nat(N).` and enter `:next` a couple of times.
You will see that the REPL starts producing results.
A system with a *Depth-first Search Strategy* like *Prolog* or *Minilog* would not be able to do so. The order in which the two clauses:

```prolog
  nat(s(N)) :- nat(N).
  nat(z).
```
appear in the knowledge base would lead to an *unproductive* infinite loop.
This is precisely the difference between having a *complete search strategy* and an incomplete one.

#### Natural Deduction Syntax

It also supports syntax of natural deduction.
Here's how one might define a factorial on *Peano Numbers* using this alternative form of syntax:

```
-------------- plus-z
plus(z, N, N)

plus(N, M, R)
-------------------- plus-suc
plus(s(N), M, s(R))

times(N, M, R)
plus(R, M, A)
------------------ times-suc
times(s(N), M, A)


--------------- times-z
times(z, _, z)

fact(N, PR)
times(s(N), PR, R)
------------------- fact-suc
fact(s(N), R)


-------------- fact-z
fact(z, s(z))
```

The key is—when the file's extension is `.nad` (for natural deduction) the `:load` command automatically knows to select the other parser and lexer.
The queries are expected to be in the same syntax as the corresponding knowledge base.


## Resources

- [Theorem Proving with Prolog](https://www.metalevel.at/prolog/theoremproving)
- [Artificial Intelligence: A Modern Approach](https://aima.cs.berkeley.edu)
