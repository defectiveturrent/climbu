More operations
**************
Mathematic operation syntax
---------------------------
Climbu has some resources that do mathematic-operation-style, such as inplicit multiplication (like a letter following a number) and more...

So, we can write ``2x`` and the multiplication stay inplicit. Here an example:
::

    >>> var x = 7 - 2
    5

    >>> 2x
    10

Can we write a sequence of letters and wait for some inplicit multiplication? No. But we can write a sequence of letters inside parentheses:
::

    >>> var x = 2
    2

    >>> var y = 5
    5

    >>> var z = 10
    10

    >>> x(y)(z)
    100

We can write this too:
::

    >>> x(y(z))
    100

Whatever way you like. Ok, so you ask me: "Is it possible to write separated variables and wait for multiplication?". The anwser for this is no. If you write, for example, ``x y z``, you'll wait for a function call, where the ``x`` is a function, and the other variables are arguments.

Multiplication using parentheses
++++++++++++++++++++++++++++++++
How you saw, we can do inplicit multiplication by putting 