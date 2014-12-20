#pragma once

#include <iostream>
#include <vector>
#include <string>

using std::cout;
using std::cin;
using std::string;
using std::vector;

enum SpecialDate_t
{
  Undefined, // Represented for undefined stuffs
  NaN,       // Not a Number
  Infinite,  // Infinite evaluation
  NuL,       // Null List
  NuT,       // Null Tuple
  NuS,       // Null String
  Null,      // Null for all
  Void       // Void space
};

using Char = char;
using Int = int;
using Float = float;
using Double = double;
using Bool = bool;

template<class T>
  using List = vector<T>;

using String = List<char>;

class ClimbuException : public std::exception
{
protected:
    string message;

public:
    ClimbuException(const string & msg)
        : std::exception(), message(msg)
    { }

    ClimbuException(const String & msg)
        : std::exception()
    {
        message = string(msg.begin(), msg.end());
    }

    const char* what() const noexcept
    {
        return message.data();
    }
};