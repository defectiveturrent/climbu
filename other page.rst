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

Arithmetic operators
++++++++++++++++++++
Climbu, like other languages, has arithmetic operators. Not only arithmetic operators, but also mathematic expressions. First, what about to take a look at the operators? ::

   >>> 4 + 2
   6

   >>> 7 - 8
   -1

   >>> 1 / 0
   Infinite

   >>> 4 * 2 ^ 2
   16

Ok, I think that's enough.

And here is a complete table of climbu operators: ::

=============  =================
  Operators       Description
-------------  -----------------
    +             Add
    -             Subtraction
    *             Times
    /             Divide
    ^             Exponential
    %             Module
=============  =================