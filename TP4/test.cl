class Main inherits IO {
	self : Int <- "oi";
	v1 : SELF_TYPE <- 3;
	main() : Int {
		v3.f4()
	};
};

class A inherits IO {
	self : SELF_TYPE;
	f1(arg1 : SELF_TYPE) : SELF_TYPE {
		let self : Int <- 2 in self
	};
	f2() : Object {self};
};

class B inherits C {
	v2 : Int;
	v3 : Int;
	f3(self : Int, arg2 : SELF_TYPE) : Object {{
		v2 <- v4@B.f2();

		(new SELF_TYPE).init(2);
	}
	};
	f4() : SELF_TYPE {{
		case v2 of
			a : Int => out_string("Int\n");
			b : String => out_string("String\n");
			c : Bool => out_string("Bool\n");
		esac;
	}};
	f5() : Object {
		v4@SELF_TYPE.f2(1)
	};
	f2(i : Int) : String {out_int(i)};
};

class C inherits A{
	v3 : Int;
	v4 : A;
	init(x : Int) : Object {{
		v3 <- x;
		self;
	}};
};

class D inherits B {

	v3 : String;
};