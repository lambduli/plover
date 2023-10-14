# Plover

Plover is a toy *automated theorem prover* based on my other small project [Minolog](https://github.com/lambduli/minilog).

The project explores a simple idea—a language like Prolog could be considered a theorem prover, we just need to use a *complete search strategy*.

Plover's syntax is a subset of Prolog's. This means it's limited to *Horn Clauses*.

The search strategy is very close to *Breadth-first Search* but not necessarily exactly it.

In ever other aspect it works exactly like the *Minilog*—it **uses unification** and is **not based** on *resolution*.

For more information about the abstract machine that both *Plover* and *Minilog* implement you can read the [write-up](https://github.com/lambduli/minilog/blob/main/WRITEUP.md) for *Minilog*.
