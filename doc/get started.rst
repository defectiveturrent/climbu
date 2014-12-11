Get started
***********
The compiler
------------
The Climbu's code is pre-compiled to an C++ code and finally compiles it to a binary file. To compile the code, Climbu uses the Clang compiler (very popular, by the way). And, for now, the interpreter is a not cool process, where the code-line is compiled to a hidden file and executes it. A new way to interpret the Climbu's code is being made.

How to use
++++++++++
To use the Climbu compilder is easy, just type on a terminal the following command: ::

   climbu -c bin file.cl

And to interpret a line, just type: ::

   climbu -i "code goes here"

You may also type the help: ::

   climbu --help

There's is no REPL yet, but I'll make it soon.

The idea of interpreting
++++++++++++++++++++++++
At first, I thought to use the Julia programming language to interpret the Climbu's code, and now I've sure I'll use it. The idea is simple: just pre-compiles the code to Julia's code and then interprets it. Ok, I know, it's not too cool, but at the moment, until I make the interpreter, I'll use Julia.

Cradle of your code
-------------------
So, can we start? Okay!

Operators
+++++++++
Climbu, like other languages, has arithmetic operators. Not only arithmetic operators, but also mathematic expressions. First, what about to take a look at the operators? ::

   >>> 4 + 2
   6

   >>> 7 - 8
   -1

   >>> 1 / 0
   Infinite

   >>> 4 * 2 ^ 2
   16

   >>> 4 % 2
   0

Ok, I think that's enough. Well, Climbu has all operators that are common in other languages (such as ``+``, ``-``, ``*``, ``/``, ``%`` etc).

Logical operators
+++++++++++++++++
We have the operators 'and' and 'or' for logic operation. ::

   >>> true and true
   true

   >>> true and false
   false

   >>> false or true
   true

   >>> true and 4
   4

Oh yeap, these operators return the second sentence when it comes right or something like. ::

   >>> false or 4
   4

   >>> false and 8
   false

   >>> true and 2
   2

   >>> true or 4
   true

To compare date, you can use the comparison operators: ::

   >>> 4 == 4
   true

   >>> 1 == 9
   false

   >>> "heya" == "heya"
   true

   >>> 4 /= 4
   false

   >>> 4 /= 3
   true

We can compare strings and lists and it'll works. ::

   >>> ['a', 'b', 'c'] == "abc"
   true

However I'll talk about it later.

Mathematic expressions
++++++++++++++++++++++
No one has ever seen anything like this (I presume): ::

   >>> var x = 2;
   2

   >>> 2x
   4

Yeap. Climbu has the mathematic-multiplication-like operation. You can suffix a variable onto a number and will multiply. You can also put parentheses to multiply: ::

   >>> (7 - 2)(4 + 2)
   30

And more: ::

   >>> var x = 2;
   2

   >>> (7 - 2)x
   10

   >>> x(7 - 2)
   10

   >>> 2(-4)
   -8

I think it is very cool.

Variables
---------
Variables in climbu hasn't to ask you which type has itself. You can declare variables without to say the type. ::

   >>> var y = 7;
   7

However, you NEED to put its value. And here you are! The compiler knows which type has that variable due to its value.

What can I put in variables?
++++++++++++++++++++++++++++
Everything. You can put numbers, strings, lists, lambdas, that is, everything.

   >>> var a = 2 + 8;
   10

   >>> var b = "Hello World";
   "Hello World"

   >>> a = 4.005;
   4.005

   >>> var list = [1, 2, 3];
   [1,2,3]

   >>> var f = lam x y -> x + y

   >>> f 1 2
   3

Since you declared the variable, you can change its value without using the keyword ``var``. Since variables are defined, you can't change its type.