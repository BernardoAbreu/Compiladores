
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
(*
class A inherits IO{
  b:Bool <- true;
  bb:Bool <- b;
  s : String <- "Hello";

  a():Int { 1} ;

};



class Main inherits A {

  c : A <- new A;

--  v1 : SELF_TYPE <- (new SELF_TYPE);
  v2 : Int <- 1 + 4;

  a():Int { 0} ;

  --b():Int {1};
*)

class Main {
  main():Object { {
    --isvoid 0;
    --not true;
    --~2;
    --1+3;
    --4-8;
    --10*3;
    --8/4;
    --1<2;
    --5<=3;
    --1 = 1;
    --(new Main) = (new Main);
    -- while true loop
    -- 5
    -- pool;
    if true then 1 else 0 fi;
    } 
  };
  
};