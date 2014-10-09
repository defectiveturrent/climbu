#pragma once

#include <iostream>
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

int print ( const string& msg )
{
  return cout << msg, 0;
}

int print ( float msg )
{
  return cout << msg, 0;
}

template<class t> int print ( const vector<t>& list )
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

template<class t> int println ( t msg )
{
  print(msg);
  return print('\n');
}

//
// Lists functions
//

template<class t, class function, class condition> vector<t> eachlist( function f, const vector<t> & list, condition d )
{
  vector<t> res;
  for ( auto l : list )
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
