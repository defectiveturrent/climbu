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
#include <cmath>
#include <vector>
#include <string>
#include <cstdint>

using std::cout;
using std::cin;
using std::string;
using std::vector;

//
// Structured definition
//

#define List vector

typedef vector<char> String;

String mkstr(const string& str )
{
  return String(str.begin(), str.end());
}

template< class t,
          class function,
          class condition >
  auto eachlist( function f, const vector<t>& list, condition d )
{
  vector<decltype(f(list[0]))> res;
  for ( auto l: list )
  {
    if(d(l))
      res.emplace_back(f(l));
  }

  return res;
}

vector<char> countlist ( char min, char max )
{
  vector<char> res;
  for ( char i = min; i <= max; ++i )
    res.emplace_back(i);

  return res;
}

vector<int> countlist ( int min, int max )
{
  vector<int> res;
  for ( int i = min; i <= max; ++i )
    res.emplace_back(i);

  return res;
}

template<class t>

  vector<t> reverse( const vector<t>& list )
{
  vector<t> res;
  for( int i = list.size() - 1; i >= 0; --i )
  {
    res.emplace_back( list[i] );
  }

  return res;
}

template<class t>
  t head( const vector<t>& list )
{
  return list[0];
}

template<class t>
  t last( const vector<t>& list )
{
  return list[list.size() - 1];
}

template<class t>
  vector<t> init( const vector<t>& list )
{
  vector<t> res = list;
  res.pop_back();
  return res;
}

template<class t>
  vector<t> tail( const vector<t>& list )
{
  vector<t> res = reverse(list);
  res.pop_back();
  res = reverse(res);
  return res;
}

template<class t>
  t index( int i, const vector<t>& list )
{
  return list[i];
}

string conc( const string& list1, const string& list2 )
{
  string res = list1;
  res += list2;

  return res;
}

template<class t>
  vector<t> conc( const vector<t>& list1, const vector<t>& list2 )
{
  vector<t> res = list1;
  for( auto l2: list2 )
  {
    res.emplace_back(l2);
  }

  return res;
}

bool elem( char element, const String & list )
{
  bool res = false;
  for( auto l: list )
    res = element == l ? true : res;

  return res;
}

template<class t>
  bool elem( t element, const vector<t> & list )
{
  bool res = false;
  for( auto l: list )
    res = element == l ? true : res;

  return res;
}

template<class t>
  vector<t> takeSince ( const vector<t>& list, int n )
{
  vector<t> res;
  for( int i = n; i < list.size(); ++i )
    res.emplace_back( list[i] );

  return res;
}

List<String> words ( const String & list )
{
  if( !elem(0x20, list)
   || !elem(0x09, list)
   || !elem(0x0B, list) )
  {
    return List<String>({list});
  }
  
  String text;
  List<String> worded;

  for( auto l: list )
  {
    if( l == 0x20 || l == 0x09 || l == 0x0B )
    {
      worded.emplace_back( text );
      text.clear();
      continue;
    }

    text.emplace_back(l);
  }

  worded.emplace_back( text );
  return worded;
}