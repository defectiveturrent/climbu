#pragma once

#include "lists.hpp"

//
// Structured definition
//

typedef int32_t USTDFUNC;

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
