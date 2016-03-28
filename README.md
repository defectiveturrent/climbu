Climbu
=======
## What is this?
My first attempt at making a compiler.

## Documentation?
It's not done yet. However, you can see the progress [here](http://climbu.readthedocs.org/en/latest/).

## Make
You must've got these dependencies:

+ ``ghc``, for compiling code
+ ``cabal``, for installing packages

The first step is to install ghc and cabal (use your whatever package manager or install it manually). Then install `missingh` from cabal (a package for haskell):

    $ cabal update
    $ cabal install missingh

Maybe cabal asks you to get a new version of itself. In this case, do what it wants and run that commands afterwards. Done that, let's compile the code:

    $ cd climbu
    $ make

Okay, done. Now you have an executable working. To see commands and other stuff, type  ``climbu --help``.

### Stuff
+ See the log [here](https://github.com/thelostt/climbu/blob/master/log.md).
+ See the doc [here](http://climbu.readthedocs.org/en/latest/).

## The license
This project is lincensed under MIT Licence, so you are free to do everything (or almost everything) with my language.
