Climbu!
=======
So, what is Climbu? You can say: "Oh, it might be just another programming language, baah, nothing important." But I say to you: "No!! Okay, it's just another programming language, but it's different." How can it be different from other languages? The Climbu is different because it is not compiled or interpreted, it's Converted.

## What? Converted? Be more specific!
Yeah, it's converted. The Climbu code, when passing by compiler, is converted to an C++ code, and finally compiled. And, again, you say: "Oh, C++? This language is so powerful..." I did think the same.

## Why are you making this language?
I like to teach a lot of nice guys around. But it's complex for him. This is the reason to make this little language, just for teaching easy (And for when I be tired to program in C++ (serious)).

## Okay, let's see it
All high-level programming languages are quite easy to learn. The most simple expressions are converted to a very complex C++ code. Let's say you want to write a generic function. Not too complex, just a little function for some data management. Using C++ you can write a quite verbose code. Meanwhile, to write the same in climbu is very simple.

Ok, taking an exemple: I want a function that plus two elements (generic).

```c++
    template<class T> t plus(T a, T b)
    {
        return a + b;
    }
```

Hmmm... Ok. Now, let's do the same using climbu:

```c++
    plus(a, b) = a + b;
```

Wow, it's fantastic! And note that this little easy code is converted to that big C++ code. The power is the same. Now look how I make a factorial function in both languages:

```c++
    int factorial(int n)
    {
        if( n <= 0 )
            return 1;

        return n * factorial(n - 1);
    }
```

Climbu:

```c++
    factorial(n) = if n <= 0
        then 1
        else n * factorial . n - 1;
```

## Documentation?
It's coming yet. Not finished. However, you can see the progress here: [Climbu documentation](http://climbu.readthedocs.org/en/latest/)

## The versions
Climbu has named versions (since v1.3) where the first named version is "I'm a zygote". This is just for fun.

## You need to read this
Well, at first, climbu wore the GNU/g++ compiler. Now it uses the clang compiler. Why? I think clang is better for this, and I like clang. So, to run climbu, you need to install clang in your computer. I know that clang is cross-platform, so it's good! You can install it in your linux or windows (I don't know about MacOS). Anyway, you can find clang here: [Clang](http://clang.llvm.org).

## The license
This project is lincensed under GPLv3, so you are free to do everything (or almost everything) with my language.
