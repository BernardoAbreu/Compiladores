
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
  b:Bool;
  s : String <- "Hello";
  a():Int { 1} ;
};
class Main inherits A {
  c : A <- new A;
  a():Int { 2} ;
  main():Int { 0 };
};

