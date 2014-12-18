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

And what I guess you're thinking:
::

    >>> var x = 2
    2

    >>> var y = 5
    5

    >>> var z = 10
    10

    >>> ((x)y)z

Yep, there's no way to time variables in sequence beyond that. Ok, so you ask me: "Is it possible to write separated variables and wait for multiplication?". The anwser for this is no. If you write, for example, ``x y z``, you'll wait for a function calling, where the ``x`` is a function, and the other variables are arguments.

You can use that in anywhere you want to! For example:
::

    >>> foo(x) = x * x
    foo defined

    >>> (5) foo 5
    125

That isn't the best way to write a multiplication, but it seems cool.