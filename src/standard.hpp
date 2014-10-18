#pragma once

#include <iostream>
#include <cmath>
#include <vector>
#include <string>

using namespace std;

//
// Print functions
//

int print ( int msg )
{
  return cout << msg, 0;
}

int print ( char msg )
{
  return cout << msg, 0;
}

int print ( const vector<char>& msg )
{
  for( auto ch: msg )
    print(ch);
  return 0;
}

int print ( float msg )
{
  return cout << msg, 0;
}

template<class t>
  int print ( const vector<t>& list )
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
  int println ( t msg )
{
  print(msg);
  return print('\n');
}

//
// Lists functions
//

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

template<class t>
  bool elem( const t& element, const vector<t>& list )
{
  bool res;
  for( auto l: list )
    res = element == l ? true : res;

  return res;
}
