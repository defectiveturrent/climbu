Climbu
******
Introduction
------------
So, what is Climbu? You can say: "Oh, it might be just another programming language, baah, nothing important." But I say to you: "No!! Okay, it's just another programming language, but it's different." How can it be different from others languages? The Climbu is different because it is not compiled or interpreted, it's Converted.

What? Converted? Be more specific!
++++++++++++++++++++++++++++++++++
Yeah, it's converted. The Climbu's code, when passing by compiler, is converted to an C++ code, and finally compiled. And, again, you say: "Oh, C++? This language is so powerful..." I did think the same.

Why are you making this language?
+++++++++++++++++++++++++++++++++
I like to teach a lot of nice guys around. But it's complex for him, because it I'm making this language, just for teaching easy (And for when I be tired to program in C++ (serious)).

Okay, let's see it
++++++++++++++++++
- It's high-level
  + Yeah, so easy to program, so natural to read, so beautiful.

- There's simple features that are tiring in C++
  + In C++ we haven't comprehension lists, there's only vectors and more vectors, for-loops and more for-loops. In Climbu, the comprehension list is easy and useful.

- Functions and Lambdas are simplified
  + In C++, the functions and lambda functions are very large and tiring to make such. However, in Climbu it's so easy than drink water. The way to make functions and lambda functions in Climbu are simplified too.

- Generic!
  + Oh yeah, generic programming in C++ is horrible in sense of large codes. In Climbu, the generic programming is in everywhere! There's no types! Yeah! YEAH! Oh... Sorry, I got excited.

- Simple expressions
  + Well, the functions' body are simple, not much complex, the language is not for large codes or whatever you think, it's to make good algorithms in the simple and better way.

The versions
++++++++++++
Climbu has named versions (since v1.3) where the first named version is "I'm a zygote". This is just for fun.

You need to read this
+++++++++++++++++++++
Well, at first, climbu wore the GNU/g++ compiler. Now it uses the clang compiler. Why? I think clang is better for this, and I like clang. So, to run climbu, you need to install clang in your computer. I know that clang is cross-platform, so it's good! You can install it in your linux or windows (I don't know about MacOS). Anyway, you can find clang here: http://clang.llvm.org .

The license
+++++++++++
This project is lincensed under GPLv3, so you are free to do everything (or almost everything) with my language.

Get started
***********
The compiler
------------
The Climbu's code is pre-compiled to an C++ code and finally compiles it to a binary file. To compile the code, Climbu uses the Clang compiler (very popular, by the way). And, for now, the interpreter is a not cool process, where the code-line is compiled to a hidden file and executes it. A new way to interpret the Climbu's code is being made.

How to use
++++++++++
To use the Climbu compilder is easy, just type on a terminal the following command:

   climbu -c bin file.cl

And to interpret a line, just type:

   climbu -i "code goes here"

You may also type the help:

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
Climbu, like other languages, has arithmetic operators. Not only arithmetic operators, but also mathematic expressions. First, what about to take a look at the operators?

   >>> 4 + 2
   6

   >>> 7 - 8
   -1

   >>> 1 / 0
   Infinite

   >>> 4 * 2 ^ 2
   16

Ok, I think that's enough.

And here is a complete table of climbu operators:

=============  =================
  Operators       Description
-------------  -----------------
=============  =================
    **'+'**             Add
    **'-'**             Subtraction
    **'*'**             Times
    **'/'**              Divide
    **'^'**              Exponential
    **'%'**              Module
=============  =================