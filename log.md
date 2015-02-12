Log
===

v1.8 (In working)
-----------------
+ Added pattern matching with lists (beta);
+ Added ``when`` to comprehension list;
+ Added some standard functions;
+ Lambdas are defined like so: ``(args) -> body``;
+ Added ``not`` expression;
+ Added ``Okay`` expression for ``True``;
+ Added an ``Option`` expression;
  - ``Some`` for box something;
  - ``None`` for nothing;
  - ``~`` for unwrapping ``Option``s;
  - ``match`` for handling ``Option``s
+ Remade some operators;
+ Some minor fixes;

v1.7
----
+ Developed an interpreter;
+ Added a REPL;
+ Removed the C++ translator;
+ Remade parser for more natural programming;
+ Removed mathematic style. It'll get back soon in future version;
+ Removed implicit string concatenation. Get back in future version;
+ Removed ``as`` tool for rethink and get back in another future version;
+ Interpreter:
  - Added lists;
  - Added tuples;
  - Added saving declarators;
+ Remade parser:
  - New way of parsing precedences;

v1.6.2
------
+ Fixed broken templates' type;
+ Remade generic types, where the respective type of an argument is its name with first letter in upper case;
+ Added a cast system (keyword: ``as``);
 
v1.6.1
------
+ Now lambdas are defined using the ``~`` prefix;
+ Fixed Bad functions' declaration;
+ Fixed the function type return;
+ Fixed other bugs;

v1.6
----
+ Added the ! operator instead of call;
+ Remade function calls with arguments (now more soft);
+ Some code improved;
+ Added (and in working) a match system;
+ Improved the function composition;
+ Improve the exceptions' system;

v1.5
----
+ Added some concepts
+ Added a syntax error handler
+ Remade for

v1.4
----
+ Remade the parentheses and lists parser.
+ Remade the tokens parser.
+ Remade tuple parser.
+ Renamed ``let in`` to ``do in``.
+ Added new way to define functions.
+ Added mathematic syntax to numbers and variables.