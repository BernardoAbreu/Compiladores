class C inherits A{  
    a : Int;
    b : Bool;
    c : SELF_TYPE;
    d : C;
    x : String;
    init(x : Int, x: Bool, y : Bool, z : SELF_TYPE, self : String) : C {
        {
            a <- x;
            b <- y;
            while (1) loop 1 pool;  --Error on loop condition type
            a <- self;
            c <- self;
            d <- self;
            z <- 1;
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
                        e : SELF_TYPE => (new B);
                esac
        };
        try():String{           --Redefinition of method
            "bye"
        };
};

class A inherits IO{
        oi():Int {
            {
                self.bye(3);
                "1";
            }
        };       --Error on return type


        bye(a :SELF_TYPE):SELF_TYPE {(new SELF_TYPE)};
};

Class Main inherits C{          
    a : Int;                    --Redefinition of inherited attribute
    b : Int;                    --Redefinition of inhreited attribute
    o : Int <- 10;
    notmain():C {{
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


        (let x : Int <- true, y : Int <- 3, o : Int <- 20, g : Int, h : SELF_TYPE in
            {
                 g <- o + y;
            }
        );

        if (2<false)
        then not 2
        else b <- a <- g        --assig undeclared g to a and object type to b
        fi;

        (new Int);
    }
    };
};


class X inherits IO {
    self : Int <- "oi";
    v1 : SELF_TYPE <- 3;
    main() : Int {
        v3.f4()
    };
};

class Y inherits IO {
    self : SELF_TYPE;
    f1(arg1 : SELF_TYPE) : SELF_TYPE {
        let self : Int <- 2 in self
    };
    f2() : Object {self};
};

class Z inherits H {
    v2 : Int;
    f3(self : Int, arg2 : SELF_TYPE) : Object {
        {
            v2 <- v4@B.f2();

            (new SELF_TYPE).init(2);
        }
    };
    
    f4() : SELF_TYPE {
        {

            case v2 of
                a : Int => out_string("Int\n");
                b : String => out_string("String\n");
                c : Bool => out_string("Bool\n");
            esac;
        }
    };
    
    f5() : Object {
        v4@SELF_TYPE.f2(1)
    };

    f6() : SELF_TYPE {
            if( 3 <= "2")
            then    self
            else    3
            fi
    };
    
    f2(i : Int) : String {
        {
            self.f3(self,5);
            out_int(i);
        }
    };

    mymeth(s: String) : String {s.a()};
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

    v3 : String;

    f5():Int{3};

    f5():Object{
        v4@SELF_TYPE.f2("hi")
    };


};