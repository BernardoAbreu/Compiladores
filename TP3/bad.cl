
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* no error *)
Class E inherits A {

	main() : Object {
		let a : Int <- 4, 
			c : Int <- -1, --error use of '-''
			d : Int <- 5 in 
			{
				out_int(a);
				out_int(c);
				out_int(d);
				case c of
				Int => out_string("Int type"); -- error. No identifier.
				b : String => out_string("String type");
				esac;

			}
	};
}
;

class F inherits IO {
	var1: Int  -- Missing semi-colon
	var2: String;
	var3: Int <- 5 <- 6; -- two assignments

	-- Missing ',' between parameters
	init(formal1: Int, formal2: Int formal3: Int, formal4: Int) : Object {};

	-- Missing type declaration
	init2(formal1: Int, formal2: Int, formal3: Int, formal4) : Object {};

	init3() : Object {
		{
			out_string("Hello");
			
			-- Missing "pool"
			while true loop out_string("");

			-- Incomplete case
			case var2 of esac;
		}
	};

	init4() : Object {
		{
			-- Missing semi-colon
			var1 <- 8
			out_int(var1);
			out_string("This is cool");
		}
	};


};

(* error:  closing brace is missing *)
Class G inherits A {
;