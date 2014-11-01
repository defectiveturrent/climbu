#pragma once
#include <iostream>
#include <cmath>
#include <vector>
#include <string>
#include <cstdint>
#include <tuple>

using namespace std;

//
// Structured definition
//

typedef int32_t USTDFUNC;

enum SpecialDate_t
{
  Undefined, // Represented for undefined stuffs
  NaN,       // Not a Number
  Infinite,  // Infinite evaluation
  NuL,       // Null List
  NuT,       // Null Tuple
  NuS,       // Null String
  Null       // Null for all
};

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

#define fst(tuple) get<0>(tuple)
#define snd(tuple) get<1>(tuple)
