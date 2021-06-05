### Generating *decent* error messages for the LR parser generator, [Menhir](http://gallium.inria.fr/~fpottier/menhir/).

Generating *decent* error messages from LR parsers generators can be quite trickly. 
Fortunately, Menhir comes with a mechanism and an incremental API to do just that.

I try to learn how to do that for OCaml's LR parser generator, Menhir.

This is adapted from the official Menhir [source](https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors/calc.ml) on GitLab.

I'll be documenting my progress on what I learn in a markdown file in this repo.

Also, I made a typo in the name of this repo (not going to correct it)
