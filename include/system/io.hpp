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

#include "../type.h"

//
// Print functions
//

SpecialDate_t putc ( char charactere );
SpecialDate_t puts ( const String & line );

SpecialDate_t print ( SpecialDate_t msg )
{
   switch( msg )
   {
      case Undefined:
         cout << "Undefined";
         break;

      case NaN:
         cout << "NaN";
         break;

      case Infinite:
         cout << "Infinite";
         break;

      case NuL:
         cout << "[]";
         break;

      case NuT:
         cout << "()";
         break;

      case NuS:
         cout << "\"\"";
         break;

      case Null:
         cout << "null";
         break;

      case Void:
         break;

      default:
         return Undefined;
   };
   return Void;
}

SpecialDate_t print ( int msg )
{
   return cout << msg, Void;
}

SpecialDate_t print ( char msg )
{
   return cout << '\'' << msg << '\'', Void;
}

SpecialDate_t print ( const String & msg )
{
   cout << '"';
   for( auto ch: msg )
      cout << ch;
   cout << '"';
   return Void;
}

SpecialDate_t print ( float msg )
{
   return cout << msg, Void;
}

template<class t> SpecialDate_t print ( const t & what )
{
   std::cout << what;
   return Void;
}

template<class t>
   SpecialDate_t print ( const List<t>& list )
{
   putc('[');
   for ( int i = 0; i < list.size(); ++i )
   {
      print(list[i]);
      if ( (i + 1) != list.size() )
         putc(',');
   }
   return putc(']');
}

template<class a, class b>
   SpecialDate_t print ( const tuple<a, b> & pair )
{
   putc('(');
   print(get<0>(pair));
   putc(',');
   print(get<1>(pair));
   return putc(')');
}

//
// PrintLn functions
//

template<class t>
   SpecialDate_t println ( t msg )
{
   print(msg);
   return putc('\n');
}

SpecialDate_t putc ( char charactere )
{
   cout << charactere;
   return Void;
}

SpecialDate_t puts ( const String & line )
{
   for( auto ch: line )
      cout << ch;
   cout << std::endl;
   return Void;
}

SpecialDate_t alert( const String & msg )
{
    return puts(msg);
}

//
// Input functions
//

String getLine ()
{
   char tmp[256];
   cin.ignore();
   cin.get( tmp, sizeof(tmp) );
   return mkstr(tmp);
}

namespace climbu
{
   template<class t> t getSomething ()
   {
      t var;
      cin >> var;
      return var;
   }
}

char getChar ()
{
   return climbu::getSomething<char>();
}

String getWord ()
{
   return mkstr(climbu::getSomething<string>());
}

int getInteger ()
{
   return climbu::getSomething<int>();
}

float getReal ()
{
   return climbu::getSomething<float>();
}