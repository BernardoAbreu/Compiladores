Class Q inherits R{
	q() : C {
	   (new C)--.init(1,true)
	};
};

class C inherits B{  
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

class R inherits Z{
        c : Int;
        d : Int <- 2;
        try():Int{
                c <- 0
        };
};

Class B inherits A {
	make():C {
	  (new C)--.init(1,true)
	};
};

Class A inherits C{
	init(x : Int, y : Bool) : A {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

