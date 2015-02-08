About lists
===========
Before you go to the market, you make a list of what you've to buy and etc. Lists are more or less like so. When we need a list, we should make this:

~~~haskell
[1, 2, 9]
~~~

Okay, we have a list of numbers now. But, what about other stuffs? I hope we'll save a lot of other stuffs too, not just numbers. So, we can!

    >>> [True, False]
    [True, False]

    >>> [1.8, 4.9]
    [1.8, 4.9]

    >>> ['A', 'B', 'c']
    "ABc"

Wow, wait. Is a list of chars equals to a string? Oh, it seems so. Okay, I think we can hold more stuffs in a list:

    >>> [ [1, 2, 3], [4, 5, 6] ]
    [[1, 2, 3], [4, 5, 6]]

    >>> [ [1..3], [4..6] ]
    [ [1, 2, 3], [4, 5, 6] ]

Alright, we've got some lists of lists of numbers and... What do that dots mean?