

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    bool main_class = false;
    
    class_map = new std::map<Symbol, Class_>();

    install_basic_classes();

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
       
        Class_ cur_class = classes->nth(i);

        Symbol cur_name = cur_class->get_name();

        // Check if Main class is present
        if (cur_name == Main){
            main_class = true;
        }

        // Check for class redefinition
        if ( class_map->find(cur_name) != class_map->end()){
            semant_error(cur_class) << "Redefinition of class " << cur_name << endl;
        }
        else{
            class_map->insert(std::make_pair(cur_name, cur_class));
        }
    }


    if (!main_class){
        semant_error() << "Missing definition of Main class." << endl;
    }
}



bool ClassTable::inheritanceUtil(Symbol vertex, bool visited[], bool *recStack,std::map<Symbol,int> *index){

    int v = (*index)[vertex];

    if(visited[v] == false){
        // Mark the current node as visited and part of recursion stack
        visited[v] = true;
        recStack[v] = true;
        
        Class_ c = (*class_map)[vertex];
        Symbol parent = c->get_parent();

        //Check Parent
        if(parent!=No_class){
            if ((parent == Int) || (parent == Str) || (parent == Bool)){
                semant_error(c) << "Invalid inheritance" << endl;
            }
            else{

                std::map<Symbol, Class_>::iterator it;
                it = class_map->find(parent);
                if (it == class_map->end()){
                    semant_error(c) << "Trying to inherit from inexistant class " << parent << endl;
                }
                else{
                    // Recur for parent
                    int p = (*index)[parent];
                    if ( !visited[p] && inheritanceUtil(parent, visited, recStack,index) )
                        return true;
                    else if (recStack[p])
                        return true;
                }
            }
        
        }
 
    }
    recStack[v] = false;  // remove the vertex from recursion stack
    return false;
}
 


// Returns true if the graph contains a cycle, else false.
bool ClassTable::inheritanceCheck(){
    // Mark all the vertices as not visited and not part of recursion
    // stack
    
    int size = class_map->size(); 
    int i;

    bool *visited = new bool[size];
    bool *recStack = new bool[size];
    std::map<Symbol,int> index;

    for(i = 0; i < size; i++)    {
        visited[i] = false;
        recStack[i] = false;
    }
 

    i = 0;
    for (std::map<Symbol, Class_>::iterator it=class_map->begin(); it!=class_map->end(); ++it){
        index[it->first] = i;
        i++;
    }

 
    for (std::map<Symbol, Class_>::iterator it=class_map->begin(); it!=class_map->end(); ++it)
        if (inheritanceUtil(it->first, visited, recStack,&index)){
            semant_error()<<"Cyclic inheritance"<<endl;
            return true;
        }
 
    return false;
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
    class_(Object, 
           No_class,
           append_Features(
                   append_Features(
                           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
           filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
    class_(IO, 
           Object,
           append_Features(
                   append_Features(
                           append_Features(
                                   single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                              SELF_TYPE, no_expr())),
                                   single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                              SELF_TYPE, no_expr()))),
                           single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
           filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
    class_(Int, 
           Object,
           single_Features(attr(val, prim_slot, no_expr())),
           filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
    class_(Str, 
           Object,
           append_Features(
                   append_Features(
                           append_Features(
                                   append_Features(
                                           single_Features(attr(val, Int, no_expr())),
                                           single_Features(attr(str_field, prim_slot, no_expr()))),
                                   single_Features(method(length, nil_Formals(), Int, no_expr()))),
                           single_Features(method(concat, 
                                      single_Formals(formal(arg, Str)),
                                      Str, 
                                      no_expr()))),
                   single_Features(method(substr, 
                              append_Formals(single_Formals(formal(arg, Int)), 
                                     single_Formals(formal(arg2, Int))),
                              Str, 
                              no_expr()))),
           filename);


    //Add basic classes to graph
    class_map->insert(std::make_pair(Object, Object_class));
    class_map->insert(std::make_pair(IO, IO_class));
    class_map->insert(std::make_pair(Int, Int_class));
    class_map->insert(std::make_pair(Bool, Bool_class));
    class_map->insert(std::make_pair(Str, Str_class));

}


Class_ ClassTable::get_Class(Symbol c){
    std::map<Symbol, Class_>::iterator it;
    it = class_map->find(c);
    if (it == class_map->end())
        return NULL;

    return it->second;
}
////////////////////////////////////////////////////////////////////
//
// classtable->semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::classtable->semant_error()                
//
//    ostream& ClassTable::classtable->semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::classtable->semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{                                                 
    semant_errors++;                            
    return error_stream;
} 


/********************************************************************
*   Semant Checking Functions
*
*
********************************************************************/

//type1 <= type2
bool type_check(Symbol type1, Symbol type2){
    return true;
}

Symbol join_types(Symbol type1, Symbol type2){
    return Object;
}

void class__class::build_features(){
    method_map = new SymbolTable<Symbol, Symbol>();
    attribute_map = new SymbolTable<Symbol, Symbol>();

    attribute_map->enterscope();
    method_map->enterscope();
}

void class__class::remove_features(){
    attribute_map->exitscope();
    method_map->exitscope();

    delete method_map;
    delete attribute_map;

}


//
//  program_class prints "program" and then each of the
//  component classes of the program, one at a time, at a
//  greater indentation. The recursive invocation on
//  "classes->nth(i)->dump_with_types(...)" shows how useful
//  and compact virtual functions are for this kind of computation.
//
//  Note the use of the iterator to cycle through all of the
//  classes.  The methods first, more, next, and nth on AST lists
//  are defined in tree.h.
//
void program_class::semant_checker(Symbol cur_class){

   for(int i = classes->first(); classes->more(i); i = classes->next(i))
      classes->nth(i)->semant_checker(cur_class);
}

//
// Prints the components of a class, including all of the features.
// Note that printing the Features is another use of an iterator.
//
void class__class::semant_checker(Symbol cur_class){

   //Create tables and check for method and attribute definition errors
   build_features();

   for(int i = features->first(); features->more(i); i = features->next(i))
      features->nth(i)->semant_checker(cur_class);

   remove_features();

}


//TODO
//SELF
// dump_with_types for method_class first prints that this is a method,
// then prints the method name followed by the formal parameters
// (another use of an iterator, this time access all of the list members
// of type Formal), the return type, and finally calls set_type recursively
// on the method body. 

void method_class::semant_checker(Symbol cur_class){
    Symbol expr_type, ret_type;

    for(int i = formals->first(); formals->more(i); i = formals->next(i))
        formals->nth(i)->semant_checker(cur_class);

    attribute_map->enterscope();
    attribute_map->addid(self, new Symbol(cur_class));
    expr->semant_checker(cur_class);

    expr_type = expr->get_type();

    if(get_return_type() == SELF_TYPE){
        ret_type = cur_class;   
    }
    else{
        ret_type = get_return_type();
    }


    type_check(expr_type, ret_type);
    attribute_map->exitscope();
}

//
//  attr_class::dump_with_types prints the attribute name, type declaration,
//  and any initialization expression at the appropriate offset.
//
void attr_class::semant_checker(Symbol cur_class){
   if(!init->isNoExpr()){
      init->semant_checker(cur_class);
      type_check(init->get_type(), get_type_decl());
   }
   
}

//
// formal_class::dump_with_types dumps the name and type declaration
// of a formal parameter.
//
void formal_class::semant_checker(Symbol cur_class){
   attribute_map->addid(name,new Symbol(type_decl));
}

//TODO
// branch_class::dump_with_types dumps the name, type declaration,
// and body of any case branch.
//
void branch_class::semant_checker(Symbol cur_class){
   if(attribute_map->probe(type_decl) != NULL){
      attribute_map->addid(type_decl, new Symbol(name));
   }
   else{
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this) << "Variable with type " << type_decl
         << " already declared on Case" << endl;
   }
   
   expr->semant_checker(cur_class);
}

//
// assign_class::dump_with_types prints "assign" and then (indented)
// the variable being assigned, the expression, and finally the type
// of the result.  Note the call to set_type (see above) at the
// end of the method.
//
void assign_class::semant_checker(Symbol cur_class){
   Symbol expr_type, var_type, finaltype;

   expr->semant_checker(cur_class);
   expr_type = expr->get_type();
   var_type = *(attribute_map->lookup(name));
   if (var_type != NULL){
      finaltype = ((type_check(expr_type, var_type)) ? expr_type : Object);
   }
   else{
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this) << "Variable " << name << " not declared" << endl;
      finaltype = Object;
   }
   
   set_type(finaltype);
}

//TODO
// static_dispatch_class::dump_with_types prints the expression,
// static dispatch class, function name, and actual arguments
// of any static dispatch.  
//
void static_dispatch_class::semant_checker(Symbol cur_class){
   expr->semant_checker(cur_class);

   for(int i = actual->first(); actual->more(i); i = actual->next(i))
     actual->nth(i)->semant_checker(cur_class);

   set_type(No_type);
}

//TODO
//   dispatch_class::dump_with_types is similar to 
//   static_dispatch_class::dump_with_types 
//
void dispatch_class::semant_checker(Symbol cur_class){
   expr->semant_checker(cur_class);

   for(int i = actual->first(); actual->more(i); i = actual->next(i))
     actual->nth(i)->semant_checker(cur_class);

   set_type(No_type);
}

//
// cond_class::dump_with_types dumps each of the three expressions
// in the conditional and then the type of the entire expression.
//
void cond_class::semant_checker(Symbol cur_class){

   pred->semant_checker(cur_class);
   then_exp->semant_checker(cur_class);
   else_exp->semant_checker(cur_class);

   if (pred->get_type() != Bool){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Predicate is not of type Bool" << endl;
   }

   set_type(join_types(then_exp->get_type(),else_exp->get_type()));
}

//
// loop_class::dump_with_types dumps the predicate and then the
// body of the loop, and finally the type of the entire expression.
//
void loop_class::semant_checker(Symbol cur_class){

   pred->semant_checker(cur_class);
   body->semant_checker(cur_class);

   if (pred->get_type() != Bool){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Predicate is not of type Bool" << endl;
   }
   set_type(Object);
}

//
//  typcase_class::dump_with_types dumps each branch of the
//  the Case_ one at a time.  The type of the entire expression
//  is dumped at the end.
//
void typcase_class::semant_checker(Symbol cur_class){
   Symbol expr_type, case_type;
   Case cur_case;

   expr->semant_checker(cur_class);

   attribute_map->enterscope();

   for(int i = cases->first(); cases->more(i); i = cases->next(i))
     cases->nth(i)->semant_checker(cur_class);

    cur_case =  cases->nth(cases->first());
    case_type = cur_case->get_expr()->get_type();

    for(int i = cases->first(); cases->more(i); i = cases->next(i)){
        cur_case =  cases->nth(i);
        expr_type = cur_case->get_expr()->get_type();
        case_type = join_types(case_type, expr_type);
    }

   attribute_map->exitscope();
     
   set_type(case_type);
}

//
//  The rest of the cases for Expression are very straightforward
//  and introduce nothing that isn't already in the code discussed
//  above.
//
void block_class::semant_checker(Symbol cur_class){
   Symbol expr_type;

   expr_type = No_type;

   for(int i = body->first(); body->more(i); i = body->next(i)){
     body->nth(i)->semant_checker(cur_class);
     expr_type = body->nth(i)->get_type();
   }
   set_type(expr_type);
}


//TODO
void let_class::semant_checker(Symbol cur_class){
   Symbol let_type;

   if(!init->isNoExpr()){
      init->semant_checker(cur_class);
   }

   if(type_decl == SELF_TYPE){
      let_type =  cur_class;
   }
   else{
      let_type = type_decl;
   }

   if(!init->isNoExpr()){
      type_check(init->get_type(),let_type);
   }

   attribute_map->enterscope();
   attribute_map->addid(identifier,new Symbol(type_decl));
   
   body->semant_checker(cur_class);

   set_type(body->get_type());

   attribute_map->exitscope();
}

void plus_class::semant_checker(Symbol cur_class){
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "First expression is not of type Int"<<endl;
   }
   if(e2->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Second expression is not of type Int"<<endl;
   }
   set_type(Int);
}

void sub_class::semant_checker(Symbol cur_class){
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "First expression is not of type Int"<<endl;
   }
   if(e2->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Second expression is not of type Int"<<endl;
   }
   set_type(Int);
}

void mul_class::semant_checker(Symbol cur_class){
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "First expression is not of type Int"<<endl;
   }
   if(e2->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Second expression is not of type Int"<<endl;
   }
   set_type(Int);
}

void divide_class::semant_checker(Symbol cur_class){
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "First expression is not of type Int"<<endl;
   }
   if(e2->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Second expression is not of type Int"<<endl;
   }
   set_type(Int);
}

void neg_class::semant_checker(Symbol cur_class){

   e1->semant_checker(cur_class);
   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Expression is not of type Int"<<endl;
   }
   
   set_type(Int);
}

void lt_class::semant_checker(Symbol cur_class){
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "First expression is not of type Int"<<endl;
   }
   if(e2->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Second expression is not of type Int"<<endl;
   }
   set_type(Bool);
}


void eq_class::semant_checker(Symbol cur_class){
   Symbol e1_type, e2_type;
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   e1_type = e1->get_type();
   e2_type = e2->get_type();


   if((e1_type == Int) || (e1_type == Str) || (e1_type == Bool)){
      if (e1_type != e2_type){
         classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Primitive types should only be compared with object of the same type"<<endl;
      }
   }
   else{
      if((e2_type == Int) || (e2_type == Str) || (e2_type == Bool)){
         classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Primitive types should only be compared with object of the same type"<<endl;
      }
   }

   set_type(Bool);
}

void leq_class::semant_checker(Symbol cur_class){
   e1->semant_checker(cur_class);
   e2->semant_checker(cur_class);

   if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "First expression is not of type Int"<<endl;
   }
   if(e2->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Second expression is not of type Int"<<endl;
   }
   set_type(Bool);
}

void comp_class::semant_checker(Symbol cur_class)
{
   
   e1->semant_checker(cur_class);
   if(e1->get_type() != Bool){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)<< "Expression is not of type Bool"<<endl;
   }
   set_type(Bool);
}

void int_const_class::semant_checker(Symbol cur_class){
   set_type(Int);
}

void bool_const_class::semant_checker(Symbol cur_class){
   set_type(Bool);
}

void string_const_class::semant_checker(Symbol cur_class){
   set_type(Str);
}


//TODO
void new__class::semant_checker(Symbol cur_class){
   Symbol type = get_type_name();
   if (type == SELF_TYPE){
      set_type(cur_class);   
   }
   else{
      set_type(type);
   }
}

void isvoid_class::semant_checker(Symbol cur_class){
   set_type(Bool);
}

void no_expr_class::semant_checker(Symbol cur_class){
   set_type(No_type);
}

void object_class::semant_checker(Symbol cur_class){
   set_type(Object);
}






/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */




void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    if(!classtable->inheritanceCheck()){
        //TODO
        //CheckFeatures
        
        //TODO
        //SetTypes
    }

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}


