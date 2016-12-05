class C inherits A{  
    a : Int;
    b : Bool;
    c : SELF_TYPE;
    d : C;
    init(x : Int, y : Bool, z : Int) : C {
        {
            a <- x;
            b <- y;
            while (false) loop 1 pool;  --Error on loop condition type
            a <- 1;
            c <- self;
            d <- self;
            z <- 1;
            self;
        }
    };
};

class B inherits A {
        var : Bool <- true;

        c : Int <- 1 + 4; --Error on arithmetic expression
        d : Int <- 0;       --Error on initialization type
        try():String{           --Error on return type
                case var of
                        a : Int =>  "Int\n";
                        a : String => "String\n";
                        c : Bool => "Bool\n";
                esac
        };

};

class A inherits IO{
        oi():Int {
            {
                self.bye(3);
                1;
            }
        };       --Error on return type


        bye(a :Int):SELF_TYPE {(new SELF_TYPE)};
};

Class Main inherits C{
    bb : Int;                    --Redefinition of inhreited attribute
    o : Int <- 10;
    main():Int {{
        if (5 = 2)           --Error on comparison expression
        then 0
        else 1
        fi;
        
        if 3 = 3                   --Error or if condition type
        then "oi" 
        else "bye"
        fi;
        
        d <- (new C);               --Error on assignement to undeclared variable       
        a <- 5 * 6;   --Error on arithmetic expression
        a <- ~a;


        (let x : Int <- 1, y : Int <- 3, o : Int <- 20, g : Int, h : SELF_TYPE in
            {
                 g <- o + y;
            }
        );

        if (2 < 8)
        then not true
        else bb <- a        --assig undeclared g to a and object type to b
        fi;

        (new Int);
    }
    };
};


class X inherits IO {
    s : Int <- 0;
    v1 : SELF_TYPE <- (new SELF_TYPE);
    main() : Object {
        v1.out_string("now")
    };
};

class Y inherits IO {
    s : SELF_TYPE;
    f1(arg1 : Bool) : SELF_TYPE {
        self
    };
    f2() : Object {self};
};

class Z inherits H {
    v2 : Int;
    v7 : D;
    f3(s : Int, arg2 : Int) : Object {
        {
            (new B)@B.oi();

            (new SELF_TYPE).init(2);
        }
    };
    
    f4() : Int {
        {
            case v2 of
                a : Int => out_string("Int\n");
                b : String => out_string("String\n");
                c : Bool => out_string("Bool\n");
                d : A => (new SELF_TYPE);
            esac;
            3;
        }
    };
    
    f5() : Object {
        v7@Z.f2()
    };

    f6() : SELF_TYPE {
            if( 3 <= 2)
            then    self
            else    s
            fi
    };

    mymeth(s: String) : String {s};
};

class H inherits Y{
    v3 : Int;
    v4 : A;
    init(x : Int) : Object {{
        v3 <- x;
        self;
    }};
};

class D inherits Z {


    f5():Object{
        {
            self@Z.f4();
            (new H)@Y.f2();
        }
    };


};