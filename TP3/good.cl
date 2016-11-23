class A {
	var : Int <- 0;


	set_var(num : Int) : SELF_TYPE {
      {
         var <- num;
         self;
      }
    };
	
	method2(num1 : Int, num2 : Int) : BB__ {  -- plus
      (let x : Int, y : Int <- 3 in
		{
            x <- num1 + num2 + y;
	    	(new BB__).set_var(x);
	 	}
      )
   };
};

Class BB__ inherits A {
	test() : String{
		case var of
			a : Int => "Int\n";
			b : String => "String\n";
			c : Bool => "Bool\n";
		esac
	};
};

Class Main inherits IO{
	var:Int;
	var2:Int;  
	var3:Int;
	var4:String;

	main() : Object {
		{
			out_string("Hello World\n");
			var2 <- let a:String <- "First", b:Int <- var <- 100, c:Bool <- true in {
				out_int(b); 
				out_string("\n");
				out_string(a);
				out_string("\n");
				if c then out_string("true\n") else out_string("false\n") fi;
				var3 <- 0;
			};
			var4 = ((new A).method2(31,var3)).test();
			out_int(var);
			out_string("\n");
			out_string(var4);
			out_string("\n");
		}
	};
};