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

//
// Print functions
//

USTDFUNC print ( int msg )
{
  return cout << msg, 0;
}

USTDFUNC print ( char msg )
{
  return cout << msg, 0;
}

USTDFUNC print ( const vector<char>& msg )
{
  for( auto ch: msg )
    print(ch);
  return 0;
}

USTDFUNC print ( float msg )
{
  return cout << msg, 0;
}

template<class t>
  USTDFUNC print ( const vector<t>& list )
{
  print('[');
  for ( int i = 0; i < list.size(); ++i )
  {
    print(list[i]);
    if ( (i + 1) != list.size() )
      print(',');
  }
  return print(']');
}

//
// PrintLn functions
//

template<class t>
  USTDFUNC println ( t msg )
{
  print(msg);
  return print('\n');
}

//
// Input functions
//

vector<char> input ()
{
  string buffer;
  cin >> buffer;

  vector<char> data(buffer.begin(), buffer.end());
  
  return data;
}
