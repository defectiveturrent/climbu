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
#include <cstdio>
#include <cstring>
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

String getLine ()
{
  char tmp[256];
  memset( tmp, 0, sizeof(tmp) );
  cin.get( tmp, sizeof(tmp) );
  cin.ignore();
  return mkstr(tmp);
}
