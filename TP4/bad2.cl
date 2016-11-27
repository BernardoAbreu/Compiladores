class C inherits A{  
    a : Int;
    b : Bool;
    init(x : Int, y : Bool) : C {
        {
            a <- x;
            b <- y;
            while (1) loop 1 pool;  --Error on loop condition type
            self;
        }
    };
};

class B inherits A {
        var : Int <- 0;
        var : Bool <- false;    --Same class attribute redefinition

        c : Int <- 1 + "Wrong"; --Error on arithmetic expression
        d : Int <- false;       --Error on initialization type
        try():String{           --Error on return type
                case var of
                        a : Int => 1;
                        a : String => "String\n";
                        c : Bool => "Bool\n";
                        d : Int => "Int\n";     --Variable with repeated type
                esac
        };
        try():String{           --Redefinition of method
            "bye"
        };
};

class A inherits IO{
        oi():Int { "1" };       --Error on return type
};

Class Main inherits C{          
    a : Int;                    --Redefinition of inherited attribute
    b : Int;                    --Redefinition of inhreited attribute
    main():C {{
        if ("a" = 2)           --Error on comparison expression
        then 0
        else 1
        fi;
        
        if 3                    --Error or if condition type
        then "oi" 
        else "bye"
        fi;
        
        d <- 5+4;               --Error on assignement to undeclared variable       
        a <- "Int " * "Bool";   --Error on arithmetic expression
        a <- ~a;
        a <- ~"a";              --Error on negative expression type

        if (2<false)
        then not 2
        else b <- a <- g        --assig undeclared g to a and object type to b
        fi;

        (new Int);
    }
    };
};
