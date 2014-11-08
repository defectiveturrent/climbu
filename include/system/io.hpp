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

typedef SpecialDate_t USTDFUNC;

//
// Print functions
//

SpecialDate_t print ( SpecialDate_t msg )
{
  switch( msg )
  {
    case Undefined:
      std::cout << "Undefined";
      break;

    case NaN:
      std::cout << "NaN";
      break;

    case Infinite:
      std::cout << "Infinite";
      break;

    case NuL:
      std::cout << "[]";
      break;

    case NuT:
      std::cout << "()";
      break;

    case NuS:
      std::cout << "\"\"";
      break;

    case Null:
      std::cout << "null";
      break;

    case Void:
      break;

    default:
      return Undefined;
  };
  return Void;
}

USTDFUNC print ( int msg )
{
  return cout << msg, Void;
}

USTDFUNC print ( char msg )
{
  return cout << msg, Void;
}

USTDFUNC print ( const vector<char>& msg )
{
  for( auto ch: msg )
    print(ch);
  return Void;
}

USTDFUNC print ( float msg )
{
  return cout << msg, Void;
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

template<class a, class b>
  USTDFUNC print ( const tuple<a, b>& pair )
{
  print('(');
  print(get<0>(pair));
  print(',');
  print(get<1>(pair));
  return print(')');
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
