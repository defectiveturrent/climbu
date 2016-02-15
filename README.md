Climbu
=======
## What is this?
Good question. Climbu is a programming language, which is focusing in the result, not in speed or something else.

Climbu is interpreted (and maybe it's fast, I don't know). Climbu mixes both imperative and functional programming language, but it can be just FP, you choose your way.

Examples:

Prints `5`:

```haskell
    foo(a, b) = a + b;

    bar = foo 1;

    do x = bar 2,
       foo = 2
    in x + foo;
```

Another example:

```haskell
    foo(f) = f 2;

    bar(x) = x + 2;

    foo (do bar(id) = id in bar); -- returns 2

    foo bar; -- returns 4

    bar = 8; -- bar now is 8
```

```haskell
    foo1 = foo(x) = x + x;
    foo2 = foo(x) = x / 2;

    do foo1,
       x = foo 4,
       foo2,
       y = foo 4

     in (x, y) -- prints (8, 2)
```

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
