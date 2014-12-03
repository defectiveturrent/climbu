/*
 *    Climbu interpreter
 *      :: TODO
 */

#include <iostream>
#include <string>
#include <vector>
#include <except>

class NoArguments : public std::exception
{
   NoArguments ()
      : std::exception();
};

int main( int argc, char** argv )
{
   try
   {
      if ( argc == 1 )
      {
         throw NoArguments();
      }

      
   }
   catch( const NoArguments& )
   {
      std::cout << "ERROR (NoArguments): May you want to put some arguments"
      return 1;
   }

   return 0;
}