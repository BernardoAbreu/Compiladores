//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#include "cool.h"
#include "tree.h"
#include "cool-tree.h"
#include "utilities.h"

// defined in stringtab.cc
void dump_Symbol(ostream& stream, int padding, Symbol b); 

// defined in cool.h
void dump_Boolean(ostream& stream, int padding, Boolean b);


void Expression_class::set_type(ostream& stream, int n)
{
  if (type)
    { stream << pad(n) << ": " << type << endl; }
  else
    { stream << pad(n) << ": _no_type" << endl; }
}

void dump_line(ostream& stream, int n, tree_node *t)
{
  stream << pad(n) << "#" << t->get_line_number() << "\n";
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
void program_class::semant_checker(){

   for(int i = classes->first(); classes->more(i); i = classes->next(i))
      classes->nth(i)->semant_checker();
}

//
// Prints the components of a class, including all of the features.
// Note that printing the Features is another use of an iterator.
//
void class__class::semant_checker(){

   //Create tables and check for method and attribute definition errors
   build_features();

   for(int i = features->first(); features->more(i); i = features->next(i))
      features->nth(i)->semant_checker();

   remove_features();

}


//TODO
//SELF
// dump_with_types for method_class first prints that this is a method,
// then prints the method name followed by the formal parameters
// (another use of an iterator, this time access all of the list members
// of type Formal), the return type, and finally calls set_type recursively
// on the method body. 

void method_class::semant_checker(){
   Symbol expr_type, ret_type;

   attribute_map->enterscope();

   for(int i = formals->first(); formals->more(i); i = formals->next(i))
      formals->nth(i)->semant_checker();

   expr->semant_checker();

   expr_type = expr->get_type();
   ret_type = get_return_type();

   type_check(ret_type,expr_type);
   attribute_map->exitscope();
}

//
//  attr_class::dump_with_types prints the attribute name, type declaration,
//  and any initialization expression at the appropriate offset.
//
void attr_class::semant_checker(){
   Symbol init_type, type;

   init->semant_checker();

   init_type = init->get_type();

   type = get_type_decl();

   type_check(type,init_type);
}

//
// formal_class::dump_with_types dumps the name and type declaration
// of a formal parameter.
//
void formal_class::semant_checker(){
   attribute_map->addid(name,type_decl);
}

//TODO
// branch_class::dump_with_types dumps the name, type declaration,
// and body of any case branch.
//
void branch_class::semant_checker(){
   if(attribute_map->probe(type_decl) != NULL){
      attribute_map->addid(type_decl,name);   
   }
   else{
      semant_error(get_filename(),this) << "Variable with type " << type_decl
         << " already declared on Case" << endl;
   }
   
   expr->semant_checker();
}

//
// assign_class::dump_with_types prints "assign" and then (indented)
// the variable being assigned, the expression, and finally the type
// of the result.  Note the call to set_type (see above) at the
// end of the method.
//
void assign_class::semant_checker(){
   Symbol expr_type, var_type;

   expr->semant_checker();
   expr_type = expr->get_type();
   var_type = attribute_map->lookup(name);
   if (var_type != NULL){
      finaltype = ((type_check(var_type, expr_type)) ? expr_type : Object);
   }
   else{
      semant_error(get_filename(),this) << "Variable " << name << " not declared" << endl;
      finaltype = Object;
   }
   
   set_type(finaltype);
}

//
// static_dispatch_class::dump_with_types prints the expression,
// static dispatch class, function name, and actual arguments
// of any static dispatch.  
//
void static_dispatch_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_static_dispatch\n";
   expr->semant_checker();
   dump_Symbol(stream, n+2, type_name);
   dump_Symbol(stream, n+2, name);
   stream << pad(n+2) << "(\n";
   for(int i = actual->first(); actual->more(i); i = actual->next(i))
     actual->nth(i)->semant_checker();
   stream << pad(n+2) << ")\n";
   set_type(stream,n);
}

//
//   dispatch_class::dump_with_types is similar to 
//   static_dispatch_class::dump_with_types 
//
void dispatch_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_dispatch\n";
   expr->semant_checker();
   dump_Symbol(stream, n+2, name);
   stream << pad(n+2) << "(\n";
   for(int i = actual->first(); actual->more(i); i = actual->next(i))
     actual->nth(i)->semant_checker();
   stream << pad(n+2) << ")\n";
   set_type(stream,n);
}

//
// cond_class::dump_with_types dumps each of the three expressions
// in the conditional and then the type of the entire expression.
//
void cond_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_cond\n";
   pred->semant_checker();
   then_exp->semant_checker();
   else_exp->semant_checker();
   set_type(stream,n);
}

//
// loop_class::dump_with_types dumps the predicate and then the
// body of the loop, and finally the type of the entire expression.
//
void loop_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_loop\n";
   pred->semant_checker();
   body->semant_checker();
   set_type(stream,n);
}

//
//  typcase_class::dump_with_types dumps each branch of the
//  the Case_ one at a time.  The type of the entire expression
//  is dumped at the end.
//
void typcase_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_typcase\n";
   expr->semant_checker();
   for(int i = cases->first(); cases->more(i); i = cases->next(i))
     cases->nth(i)->semant_checker();
   set_type(stream,n);
}

//
//  The rest of the cases for Expression are very straightforward
//  and introduce nothing that isn't already in the code discussed
//  above.
//
void block_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_block\n";
   for(int i = body->first(); body->more(i); i = body->next(i))
     body->nth(i)->semant_checker();
   set_type(stream,n);
}

void let_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_let\n";
   dump_Symbol(stream, n+2, identifier);
   dump_Symbol(stream, n+2, type_decl);
   init->semant_checker();
   body->semant_checker();
   set_type(stream,n);
}

void plus_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_plus\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}

void sub_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_sub\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}

void mul_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_mul\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}

void divide_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_divide\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}

void neg_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_neg\n";
   e1->semant_checker();
   set_type(stream,n);
}

void lt_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_lt\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}


void eq_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_eq\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}

void leq_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_leq\n";
   e1->semant_checker();
   e2->semant_checker();
   set_type(stream,n);
}

void comp_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_comp\n";
   e1->semant_checker();
   set_type(stream,n);
}

void int_const_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_int\n";
   dump_Symbol(stream, n+2, token);
   set_type(stream,n);
}

void bool_const_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_bool\n";
   dump_Boolean(stream, n+2, val);
   set_type(stream,n);
}

void string_const_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_string\n";
   stream << pad(n+2) << "\"";
   print_escaped_string(stream,token->get_string());
   stream << "\"\n";
   set_type(stream,n);
}

void new__class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_new\n";
   dump_Symbol(stream, n+2, type_name);
   set_type(stream,n);
}

void isvoid_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_isvoid\n";
   e1->semant_checker();
   set_type(stream,n);
}

void no_expr_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_no_expr\n";
   set_type(stream,n);
}

void object_class::semant_checker()
{
   dump_line(stream,n,this);
   stream << pad(n) << "_object\n";
   dump_Symbol(stream, n+2, name);
   set_type(stream,n);
}

