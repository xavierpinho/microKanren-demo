# microKanren-demo

A port of μKanren to Haskell.

## Quick Start

Load `MicroKanren`:

    ghci MicroKanren.hs
	
and try a few examples:

	*MicroKanren> run' [0] $ exists (\x -> membero x (list [sym "X", sym "Y"]))
	[[(0,X)],[(0,Y)]]
	
	*MicroKanren> run' [0,1] $ exists2 (\x y -> appendo x y (list [sym "X", sym "Y"]))
	[[(0,Nil),(1,Cons[X,Cons[Y,Nil]])],[(0,Cons[X,Nil]),(1,Cons[Y,Nil])],[(0,Cons[X,Cons[Y,Nil]]),(1,Nil)]]
	
	
## Literate Haskell

`MicroKanren.hs` is automatically produced from `MicroKanren.org`, a (very incomplete) *Literate Haskell* program written in *Org Mode*.

To build `MicroKanren.hs`, open `MicroKanren.org` with Emacs and run `org-babel-tangle`.
	
## License

Public domain (see the `UNLICENSE` file.)

## References

* μKanren: A Minimal Functional Core for Relational Programming. See: [HemannMuKanren2013.pdf](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
