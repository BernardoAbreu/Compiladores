(*
 *  TP1 Compiladores
 *  Bernardo de Almeida Abreu
 *  Matricula: 20130071533
 *
 *)

class Stack {
   -- Operacoes na pilha vazia

   isNil() : Bool { true };

   peek()  : String { { abort(); ""; } };

   pop()  : Stack { { abort(); self; } };

   push(s : String) : Stack {
      (new Cons).init(s, self)
   };

};


class Cons inherits Stack {

   cell : String;	-- Elemento da celula

   next : Stack;

   isNil() : Bool { false };

   peek()  : String { cell };

   pop()  : Stack { next };

   init(s : String, rest : Stack) : Stack {
      {
	 	cell <- s;
	 	next <- rest;
	 	self;
      }
   };

};


class StackCommand inherits IO{
	command : String;

	init(c : String) : StackCommand {

		if c = "e" then (new ExecuteCommand) else
		if c = "d" then (new DisplayCommand) else 
			{ command <- c; self;}
		fi fi
	};

	execute(s : Stack) : Stack { s.push(command) };
};


class ExecuteCommand inherits StackCommand {
	str1 : String;
	str2 : String;
	atooi : A2I;

	execute(s : Stack) : Stack{
		if s.isNil() then s else
			if s.peek() = "+" then{
				atooi <- new A2I;
				s <- s.pop();
				str1 <- s.peek();
				s <- s.pop();
				str2 <- s.peek();
				s <- s.pop();
				s <- s.push(atooi.i2a(atooi.a2i(str1)+atooi.a2i(str2)));
			}
			else
				if s.peek() = "s" then{
					s <- s.pop();
					str1 <- s.peek();
					s <- s.pop();
					str2 <- s.peek();
					s <- s.pop();
					s <- s.push(str1).push(str2);
				}
				else s fi
			fi
		fi
	};
};

class DisplayCommand inherits StackCommand {
	execute(s : Stack) : Stack{
  		if not s.isNil() then {
			out_string(s.peek());
			out_string("\n");
			self.execute(s.pop());
			s;
		}
		else s fi
   };
};


class Main inherits IO {
   	mystack : Stack <- new Stack;
   	command : String;
	com : StackCommand <- new StackCommand;

   	main() : Object {
		while not {out_string(">"); command <- in_string();}= "x" loop
			mystack <- (com.init(command)).execute(mystack)
		pool
   };
};
