class C inherits A{  
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class B inherits A {
        var : Int <- 0;
        c : Int;
        d : Int <- 2;
        try():String{
                case var of
                        a : Int => "Int\n";
                        a : String => "String\n";
                        c : Bool => "Bool\n";
                esac
        };
};

class A inherits IO{
        oi():Int { 1 };
};

Class Main inherits C{
	main():C {
	  (new C).init(1,true)
	};
};
