/*
    Climbu compiler / interpreter
    Copyright (C) 2014  Mario Feroldi

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#pragma once

#include <iostream>
#include <functional>
#include <cmath>
#include <vector>
#include <string>
#include <cstdint>
#include <tuple>

using std::cout;
using std::cin;
using std::string;
using std::vector;
using std::get;
using std::pair;
using std::tuple;

using namespace std::placeholders;

//
// Structured definition
//

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

typedef char Char;
typedef int Int;
typedef float Float;
typedef double Double;
typedef bool Bool;

#include "lists.hpp"
#include "system/io.hpp"

template<class t>
  t sum( const vector<t>& list )
{
  t res;
  for( auto l: list )
    res += l;

  return res;
}

template<class t>
  t product( const vector<t>& list )
{
  t res;
  for( auto l: list )
    res *= l;

  return res;
}

int length ( char* data )
{
  for( int count = 1; ; ++count )
  {
    if( data[count] == 0x0 )
      return count;
  }
  return 0;
}

template<class t>
  t max( t a, t b )
{
  return a > b ? a : b;
}

template<class t>
  t min( t a, t b )
{
  return a < b ? a : b;
}

#define fst(tuple) get<0>(tuple)
#define snd(tuple) get<1>(tuple)