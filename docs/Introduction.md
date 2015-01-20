Everything is cool
==================
Really, everything is cool. Let's begin with 'what we can do' in Climbu.

## Explaining the basics
We have:

+ Numbers;
+ Characters;
+ Lists:
  - Strings;
+ Tuples
+ Functions & lambda;
+ And more expressions...

Done.

Numbers
-------
Climbu has every number. Every each number. Integers and floating points.

Characters
----------
Climnu has characters too. Like this: ``'a'``.

Lists
-----
Climbu has a list for every each data type. Lists in Climbu are equal in Haskell, Rust, Python etc.

    [1, 2, 3] // Cool list of numbers

    [1..3] // Cool range of numbers (this is equals to [1, 2, 3])

Climbu has a comprehension list too, but it's a subject for another topic.

Lists can hold lists too, like an inception: ``[[[["Such lists!"]]]]``.

And, finally, strings are a list of characters. Yes, you can make a literal string, using a quotation mark. The point is that the literal string is converted to a list of characters. In other words, ``"hi"`` is equals to ``['h', 'i']`` and vise-versa.

Tuples
------
Just like haskell, tuples are defined with parentheses and commas between the content. Like so: ``(2, 'c', [1, 1, 1])``.

Functions & lambda
------------------
We can declare functions easily. Like so:

    add(a, b) = a + b;

And using it like so:

    add 4 6;

To declare, we use parentheses and commas (like tuples), and its name before. After that, we put the sign ``=`` (equal) and its content after. An important stuff here is that Climnu has no statements. Climbu is entirely made of expressions. So, everything returns something, even functions declaration.

Okay, now about lambda functions: likely functions, we use tuple's syntax to declare our parameters, but instead of the sign of equal, we use a right arrow (``->``). Like so:

    (a, b) -> a + b;

You can save this into a variable and use it like a function.

