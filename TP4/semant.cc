

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

    bool main_class = false;
    error = false;
    
    class_map = new std::map<Symbol, Class_>();

    install_basic_classes();

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
       
        Class_ cur_class = classes->nth(i);

        Symbol cur_name = cur_class->get_name();

        // Check if Main class is present
        if (cur_name == Main){
            main_class = true;
        }

        if(cur_name == SELF_TYPE){
          semant_error(cur_class) << "Class cannot have name SELF_TYPE." << endl;
        }
        else if ( class_map->find(cur_name) != class_map->end()){ // Check for class redefinition
            semant_error(cur_class) << "Class " << cur_name 
            << " was previously defined." << endl;
            error = true;
        }
        else{
            class_map->insert(std::make_pair(cur_name, cur_class));
        }
    }


    if (!main_class){
        semant_error() << "Class Main is not defined." << endl;
    }
}



bool ClassTable::check_Cycle(Symbol vertex, bool visited[], bool *recStack, std::map<Symbol,int> *index){

    int v = (*index)[vertex];

    if(!visited[v]){
        // Mark the current node as visited and part of recursion stack
        visited[v] = true;
        recStack[v] = true;
        
        Class_ c = (*class_map)[vertex];
        Symbol parent = c->get_parent();

        //Check Parent
      if((parent != No_class) && (parent != Int) && (parent != Str) && (parent != Bool) && (parent != SELF_TYPE)){
          if (class_map->find(parent) != class_map->end()){
              // Recur for parent
              int p = (*index)[parent];
              if ((!visited[p] && check_Cycle(parent, visited, recStack,index) ) || (recStack[p])){
                  semant_error(c)<< "Class " << vertex
                    << ", or an ancestor of " << vertex
                    << ", is involved in an inheritance cycle." << endl;
                  return true;
              }
          }
        
      }
 
    }
    recStack[v] = false;  // remove the vertex from recursion stack
    return false;
}


bool ClassTable::check_Parent(Class_ c){
  Symbol parent = c->get_parent();
  Symbol class_name = c->get_name();

  if(parent != No_class){
    if ((parent == Int) || (parent == Str) || (parent == Bool) || (parent == SELF_TYPE)){
      semant_error(c) << "Class " << class_name
      << " cannot inherit class " << parent << "." << endl;
      return true;
    }
    else{
      if (class_map->find(parent) == class_map->end()){
          semant_error(c) << "Class " << class_name
            << " inherits from an undefined class " << parent << "." << endl;
          return true;
      }
    }
  }
  return false;
}

// Returns true if the graph contains a cycle, else false.
bool ClassTable::inheritanceErrorCheck(){
    // Mark all the vertices as not visited and not part of recursion
    // stack
    
    int size = class_map->size(); 
    int i;

    bool *visited = new bool[size];
    bool *recStack = new bool[size];
    std::map<Symbol,int> index;

    for(i = 0; i < size; i++){
        visited[i] = false;
        recStack[i] = false;
    }

    i = 0;
    for (std::map<Symbol, Class_>::iterator it=class_map->begin(); it!=class_map->end(); ++it){
        index[it->first] = i;
        i++;
    }
    
    for (std::map<Symbol, Class_>::iterator it=class_map->begin(); it!=class_map->end(); ++it)
        if (check_Parent(it->second) || check_Cycle(it->first, visited, recStack,&index)){
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


/*******************************************************************
*   Helper Functions
*   
********************************************************************/

//type1 <= type2
bool type_check(Symbol type1, Symbol type2){

    Symbol small_class = type1;
    Symbol big_class = type2;

    if (big_class == Object){
      return true;
    }

    while(small_class != Object){
      if(small_class == big_class){
        return true;
      }
      else{
          small_class = (classtable->get_Class(small_class))->get_parent();
      }
    }
    return false;
}

Symbol join_types(Symbol type1, Symbol type2){

    if(type1 == Object || type2 == Object) return Object;

    Symbol class_name = type1;

    while(class_name != Object){
      if(type_check(type2,class_name))
        return class_name;
      else
        class_name = (classtable->get_Class(class_name))->get_parent();
    }

    return Object;
}

void method_class::add_feature(Symbol c){
    Symbol name = get_name();
    Symbol *meth_type;

    if (method_map->probe(name) != NULL){
      classtable->semant_error(classtable->get_Class(c)->get_filename(), this)
        << "Method " << name << " is multiply defined." << endl;
    }
    else{

      meth_type = method_map->lookup(name);

      if((meth_type != NULL) && ((*meth_type) != return_type)){
          classtable->semant_error(classtable->get_Class(c)->get_filename(), this)
            << "In redefined method " << name 
            <<", return type " << return_type << " is different from original return type "
            << *meth_type << "." << endl;
      }
      else{
        method_map->addid(name, &return_type);
      }      
      
    }
}

void attr_class::add_feature(Symbol c){
    Symbol name = get_name();

    if(name != self){
      if (attribute_map->lookup(name) != NULL){
        if(attribute_map->probe(name) != NULL){
          classtable->semant_error(classtable->get_Class(c)->get_filename(), this)
            << "Attribute " << name
            << " is multiply defined in class." << endl;
        }
        else{
          classtable->semant_error(classtable->get_Class(c)->get_filename(), this)
            << "Attribute " << name
            << " is an attribute of an inherited class." << endl;
        }

      }
      else{
        attribute_map->addid(name, &type_decl);
      }
    }


}

void build_feat(Symbol c){
  Symbol name, feat_type;
  Features features;
  Class_ cur_class;

  if(c == No_class) return;

  cur_class = classtable->get_Class(c);

  build_feat(cur_class->get_parent());

  method_map->enterscope();
  attribute_map->enterscope();
    
  features = cur_class->get_features();

  for(int i = features->first(); features->more(i); i = features->next(i)){
      Feature feat = features->nth(i);
      name = feat->get_name();
      feat_type = feat->get_type();
      if(feat->isMethod()){
          Symbol *meth_type;
          if ((method_map->probe(name) == NULL) && (((meth_type = method_map->lookup(name)) == NULL) || (*meth_type == feat_type))){
              method_map->addid(name, new Symbol(feat_type));  
          }
      }
      else{
          if((name != self) && (attribute_map->lookup(name) == NULL)){
              attribute_map->addid(name, new Symbol(feat_type));
          }
      }
  }
}


void build_features(Symbol c){
    method_map = new SymbolTable<Symbol, Symbol>();
    attribute_map = new SymbolTable<Symbol, Symbol>();

    bool is_Main = false;

    Class_ cur_class = classtable->get_Class(c);

    build_feat(cur_class->get_parent());

    method_map->enterscope();
    attribute_map->enterscope();
    
    Features features = cur_class->get_features();
    for(int i = features->first(); features->more(i); i = features->next(i)){
        features->nth(i)->add_feature(c);
        Feature feat = features->nth(i);
        if(c == Main){
          is_Main |= (feat->isMethod() && (feat->get_name() == main_meth));
        }
    }

    if((c == Main) && !is_Main){
      classtable->semant_error(cur_class)<< "No 'main' method in class Main." << endl;
    }

    attribute_map->addid(self, new Symbol(SELF_TYPE));

}

void remove_features(){
    attribute_map->exitscope();
    method_map->exitscope();

    delete method_map;
    delete attribute_map;

}

method_class* find_Method(Class_ c, Symbol method_name){

    method_class* meth = NULL;
    Symbol class_name = c->get_name();

    while(class_name != No_class){
      Features features = c->get_features();

      for(int i = features->first(); features->more(i); i = features->next(i)){
        Feature feat = features->nth(i);
        if( feat->isMethod() && (feat->get_name() == method_name)){
          return (method_class*)feat;
        }
      }

      class_name = c->get_parent();
      c = classtable->get_Class(c->get_parent());
    }

    return meth;
}


/***********************************************************************/


void program_class::semant_checker(){

   for(int i = classes->first(); classes->more(i); i = classes->next(i))
      classes->nth(i)->semant_checker();
}


void class__class::semant_checker(){

   //Create tables and check for method and attribute definition errors
   build_features(get_name());

   for(int i = features->first(); features->more(i); i = features->next(i)){
      //cerr << i << endl;
      features->nth(i)->semant_checker(get_name());
   }

   remove_features();

}


void method_class::semant_checker(Symbol cur_class){
    Symbol expr_type, ret_type;

    for(int i = formals->first(); formals->more(i); i = formals->next(i))
        formals->nth(i)->semant_checker(cur_class);

    attribute_map->enterscope();

    expr->semant_checker(cur_class);

    expr_type = (expr->get_type() == SELF_TYPE) ? cur_class : expr->get_type();
    ret_type = (return_type == SELF_TYPE) ? cur_class : return_type;
    
    if (!type_check(expr_type, ret_type)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Inferred return type "<< expr->get_type() << " of method " << get_name()
        << " does not conform to declared return type " << return_type << "." << endl;
    }

    attribute_map->exitscope();
}


void attr_class::semant_checker(Symbol cur_class){
    Symbol init_type, attr_type;

    if (name == self){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(), this)
          << "'self' cannot be the name of an attribute." << endl;  
    }
    
    init->semant_checker(cur_class);
    if(init->get_type() != No_type){
      //init_type = init->get_type();
      init_type = (init->get_type() == SELF_TYPE) ? cur_class : init->get_type();
      attr_type = (type_decl == SELF_TYPE) ? cur_class : type_decl;

      if (!type_check(init_type, attr_type)){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
          << "Inferred type "<< init->get_type()
          << " of initialization of attribute " << name
          << " does not conform to declared type " << type_decl << "." << endl;
      }
    }
}


void formal_class::semant_checker(Symbol cur_class){
    bool error = false;
    if (name == self){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "'self' cannot be the name of a formal parameter." << endl;
      error = true;
    }

    if(type_decl == SELF_TYPE){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Formal parameter " << name << " cannot have type SELF_TYPE." << endl;

      error = true;
    }
    
    if (!error){
      attribute_map->addid(name,new Symbol(type_decl));
    }
}


void branch_class::semant_checker(Symbol cur_class){

	if(attribute_map->probe(type_decl) != NULL){
    	classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this) 
        	<< "Duplicate branch " << type_decl
        	<< " in case statement." << endl;
   	}
   	if(type_decl == SELF_TYPE){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this) 
        	<< "Identifier " << name
        	<< " declared with type SELF_TYPE in case branch." << endl;
	}
	
	attribute_map->addid(type_decl, &name);

   
   attribute_map->enterscope();
   attribute_map->addid(name, &type_decl);
   expr->semant_checker(cur_class);
   attribute_map->exitscope();
}


void assign_class::semant_checker(Symbol cur_class){
    Symbol expr_type, attr_type, finaltype;
    Symbol *var_type;

    expr->semant_checker(cur_class);
    
    var_type = (attribute_map->lookup(name));
    
    if (var_type != NULL){
      expr_type = (expr->get_type() == SELF_TYPE) ? cur_class : expr->get_type();
      attr_type = (*var_type == SELF_TYPE) ? cur_class : *var_type;
    
      if (!type_check(expr_type, attr_type)){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
          << "Type " << expr->get_type() 
          << " of assigned expression does not conform to declared type "
          << *var_type << " of identifier " << name << "." << endl;
      }

      finaltype = expr_type;
    }
    else{
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Assignment to undeclared variable " << name << "." << endl;

      finaltype = Object;
    }
    
    set_type(finaltype);
}


void static_dispatch_class::semant_checker(Symbol cur_class){
    int i, j;
    Symbol expr_type, actual_type, formal_type, dispatch_type, method_type;
    method_class *method;
    Formal  formal;
    Class_ c;

    dispatch_type = Object;
    expr->semant_checker(cur_class);
    expr_type = expr->get_type();
    
    
    //Static only
    /*******************************************************/
    if(type_name == SELF_TYPE){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
          << "Static dispatch to SELF_TYPE." << endl;
    }
    /*******************************************************/
    else if ((c = classtable->get_Class(type_name)) == NULL){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this) << "Class " << type_name
          << " is undefined." << endl;
    }
    else{
        //Static only
        /*******************************************************/
        if(!type_check(expr_type, type_name)){
            classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
              << "Expression type " << expr_type
              << " does not conform to declared static dispatch type " << type_name << "." << endl;
            
        }
        /*******************************************************/
        else{
            method = find_Method(c,name);

            if(method == NULL){
                classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
                  << "Dispatch to undefined method " << name << "." << endl;
            }
            else{
                method_type = method->get_type();
                Formals formals = method->get_formals();

        
                for(i = actual->first(), j = formals->first(); (actual->more(i)) && (formals->more(j)); i = actual->next(i), j = formals->next(j)){
                  actual->nth(i)->semant_checker(cur_class);
                  actual_type = actual->nth(i)->get_type();

                  formal =  formals->nth(j);
                  formal_type = formal->get_type_decl();

                  if(!type_check(actual_type, formal_type)){
                      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
                        << "In call of method " << name << ", type " << actual_type
                        << " of parameter " << formal->get_name()
                        << " does not conform to declared type " << formal_type << "." << endl;
                  }
                }

                if ( (actual->more(i)) || (formals->more(j)) ){
                    classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
                      << "Method " << name << " called with wrong number of arguments." << endl;
                }

                dispatch_type = (method_type == SELF_TYPE) ? expr_type : method_type;
          }
        
        }
    }

    set_type(dispatch_type);
}


void dispatch_class::semant_checker(Symbol cur_class){
    int i, j;
    Symbol expr_type, actual_type, formal_type, dispatch_type, method_type, type_name;
    method_class *method;
    Formal  formal;
    Class_ c;

    dispatch_type = Object;

    expr->semant_checker(cur_class);
    expr_type = expr->get_type();


    type_name = (expr_type == SELF_TYPE) ? cur_class : expr_type;

    if ((c = classtable->get_Class(type_name)) == NULL){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
          << "Class " << type_name
          << " is undefined." << endl;
    }
    else{
        method = find_Method(c,name);

        if(method == NULL){
            classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
              << "Dispatch to undefined method " << name << "." << endl;
        }
        else{
            method_type = method->get_type();
            Formals formals = method->get_formals();

            for(i = actual->first(), j = formals->first(); (actual->more(i)) && (formals->more(j)); i = actual->next(i), j = formals->next(j)){
              actual->nth(i)->semant_checker(cur_class);
              actual_type = actual->nth(i)->get_type();

              formal =  formals->nth(j);
              formal_type = formal->get_type_decl();

              if(!type_check(actual_type, formal_type)){
                  classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
                    << "In call of method " << name << ", type " << actual_type
                    << " of parameter " << formal->get_name()
                    << " does not conform to declared type " << formal_type << "." << endl;
              }
            }

            if ( (actual->more(i)) || (formals->more(j)) ){
                classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
                  << "Method " << name << " called with wrong number of arguments." << endl;
            }

            dispatch_type = (method_type == SELF_TYPE) ? expr_type : method_type;
        }
    }

    set_type(dispatch_type);
}


void cond_class::semant_checker(Symbol cur_class){

    pred->semant_checker(cur_class);
    then_exp->semant_checker(cur_class);
    else_exp->semant_checker(cur_class);

    if (pred->get_type() != Bool){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Predicate of 'if' does not have type Bool." << endl;
    }

    set_type(join_types(then_exp->get_type(),else_exp->get_type()));
}

void loop_class::semant_checker(Symbol cur_class){

    pred->semant_checker(cur_class);
    body->semant_checker(cur_class);

    if (pred->get_type() != Bool){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Loop condition does not have type Bool." << endl;
    }
    set_type(Object);
}

void typcase_class::semant_checker(Symbol cur_class){
    Symbol expr_type, case_type;
    Case cur_case;


    expr->semant_checker(cur_class);

    attribute_map->enterscope();

    for(int i = cases->first(); cases->more(i); i = cases->next(i))
        cases->nth(i)->semant_checker(cur_class);

    cur_case =  cases->nth(cases->first());
    case_type = cur_case->get_expr()->get_type();
    case_type = (case_type == SELF_TYPE) ? cur_class : case_type;
  
    for(int i = cases->first(); cases->more(i); i = cases->next(i)){
        cur_case =  cases->nth(i);
        expr_type = cur_case->get_expr()->get_type();
        expr_type = (expr_type == SELF_TYPE) ? cur_class : expr_type;
        case_type = join_types(case_type, expr_type);
    }

    attribute_map->exitscope();

    set_type(case_type);
}


void block_class::semant_checker(Symbol cur_class){
    Symbol expr_type = No_type;

    for(int i = body->first(); body->more(i); i = body->next(i)){
      body->nth(i)->semant_checker(cur_class);
      expr_type = body->nth(i)->get_type();
    }

    set_type(expr_type);
}


void let_class::semant_checker(Symbol cur_class){
    Symbol init_type, let_type;

    init->semant_checker(cur_class);

    if(init->get_type() != No_type){
      init_type = (init->get_type() == SELF_TYPE) ? cur_class : init->get_type();
      let_type = (type_decl == SELF_TYPE) ? cur_class : type_decl;
      if (!type_check(init_type,let_type)){
        classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
          << "Inferred type " << init->get_type() << " of initialization of "
          << identifier << " does not conform to identifier's declared type "
          << type_decl << "." << endl;
      }
    }

    attribute_map->enterscope();

    if(identifier == self){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "'self' cannot be bound in a 'let' expression." << endl;
    }
    else{
      attribute_map->addid(identifier,new Symbol(type_decl));
    }

    body->semant_checker(cur_class);

    set_type(body->get_type());

    attribute_map->exitscope();
}

void plus_class::semant_checker(Symbol cur_class){
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    if((e1->get_type() != Int) || (e2->get_type() != Int)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "non-Int arguments on arithmetic expression." << endl;
    }
    
    set_type(Int);
}

void sub_class::semant_checker(Symbol cur_class){
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    if((e1->get_type() != Int) || (e2->get_type() != Int)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "non-Int arguments on arithmetic expression." << endl;
    }

    set_type(Int);
}

void mul_class::semant_checker(Symbol cur_class){
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    if((e1->get_type() != Int) || (e2->get_type() != Int)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "non-Int arguments on arithmetic expression." << endl;
    }
    
    set_type(Int);
}

void divide_class::semant_checker(Symbol cur_class){
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    if((e1->get_type() != Int) || (e2->get_type() != Int)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "non-Int arguments on arithmetic expression." << endl;
    }
    
    set_type(Int);
}

void neg_class::semant_checker(Symbol cur_class){

    e1->semant_checker(cur_class);
    if(e1->get_type() != Int){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Argument of '~' has type "
        << e1->get_type() << " instead of Int." << endl;
    }

    set_type(Int);
}

void lt_class::semant_checker(Symbol cur_class){
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    if((e1->get_type() != Int) || (e2->get_type() != Int)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "non-Int arguments on '<' expression." << endl;
    }

    set_type(Bool);
}


void eq_class::semant_checker(Symbol cur_class){
    Symbol e1_type, e2_type;
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    e1_type = e1->get_type();
    e2_type = e2->get_type();


    if((e1_type == Int) || (e1_type == Str) || (e1_type == Bool) ||
        (e2_type == Int) || (e2_type == Str) || (e2_type == Bool)){

      if (e1_type != e2_type){
          classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
            << "Illegal comparison with a basic type." << endl;
      }
    }

    set_type(Bool);
}

void leq_class::semant_checker(Symbol cur_class){
    e1->semant_checker(cur_class);
    e2->semant_checker(cur_class);

    if((e1->get_type() != Int) || (e2->get_type() != Int)){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "non-Int arguments on  '<=' expression." << endl;
    }
    set_type(Bool);
}

void comp_class::semant_checker(Symbol cur_class){
   
    e1->semant_checker(cur_class);
    if(e1->get_type() != Bool){
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Argument of 'not' has type "
        << e1->get_type() << " instead of Bool." << endl;
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

void new__class::semant_checker(Symbol cur_class){
    set_type(get_type_name());
}

void isvoid_class::semant_checker(Symbol cur_class){
    set_type(Bool);
}

void no_expr_class::semant_checker(Symbol cur_class){
    set_type(No_type);
}

void object_class::semant_checker(Symbol cur_class){
    Symbol *var_type;
    var_type = attribute_map->lookup(name);
    if(var_type != NULL){
      set_type(*var_type);
    }
    else{
      classtable->semant_error(classtable->get_Class(cur_class)->get_filename(),this)
        << "Undeclared identifier " << name << "." << endl;
      set_type(Object);
    }
    
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
    if(!classtable->hasError() && !classtable->inheritanceErrorCheck()){
        semant_checker();
    }

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

