
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <sstream>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(reg,0,SP,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}



// My emit functions
///////////////////////////////////////////////////////////
static void emit_jal_init(Symbol sym,ostream &s)
{ s << JAL; emit_init_ref(sym, s); s << endl; }
///////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      emit_disptable_ref(Str,s);  s << endl;                  // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      emit_disptable_ref(Int,s);
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      emit_disptable_ref(Bool,s);
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  

   label_index = 0;

   class_table = new SymbolTable<Symbol, obj_elem>();

   enterscope();

   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   class_table->enterscope();

   tags = new Symbol[number_of_classes];
   number_of_classes = 0;
   set_tags(root());

   stringclasstag =  class_table->lookup(Str)->tag;
   intclasstag =     class_table->lookup(Int)->tag;
   boolclasstag =    class_table->lookup(Bool)->tag;
   code();
   class_table->exitscope();
   exitscope();
   delete tags;
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
  new CgenNode(class_(No_class,No_class,nil_Features(),filename),
          Basic,this));
  addid(SELF_TYPE,
  new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
          Basic,this));
  addid(prim_slot,
  new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
          Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
     No_class,
     append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
     filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
     filename),     
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
      Object,
            single_Features(attr(val, prim_slot, no_expr())),
      filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
       filename),
        Basic,this));

   number_of_classes = 5;
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);

  number_of_classes++;
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



//////////////////////////////////////////////////////////////
///My functions

//Returns number of attributes inserted on table
int build_attribute_table(CgenNodeP nd){

    Symbol class_name = nd->get_name();

    if( class_name == No_class){
      return 0;
    }

    int attr_n = build_attribute_table(nd->get_parentnd());

    Features feats = nd->features;
    Feature feat;

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      feat = feats->nth(i);
      if(!feat->is_method()){
        attribute_table->addid(feat->get_name(), new attr_elem{feat->get_type(),attr_n + DEFAULT_OBJFIELDS,SELF});
        attr_n++;
      }
    }
    return attr_n;
}



void CgenClassTable::set_tags(CgenNodeP nd){

    class_table->addid(nd->get_name(), new obj_elem{number_of_classes,nd});
    tags[number_of_classes] = nd->get_name();
    number_of_classes++;
    for(List<CgenNode> *l = nd->get_children(); l; l = l->tl()){
      set_tags(l->hd());
    }
}



void code_disp_list(CgenNodeP nd, std::list<std::pair<Symbol,Symbol> > *meth_list){
    Symbol class_name = nd->get_name();
    if( class_name != Object){
        code_disp_list(nd->get_parentnd(), meth_list);
    }

    Features feats = nd->features;
    Feature feat;
    
    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
        feat = feats->nth(i);

        if(feat->is_method()){
            bool inserted = false;

            for (std::list<std::pair<Symbol,Symbol> >::iterator it=meth_list->begin(); it != meth_list->end(); ++it){
                if((*it).second == feat->get_name()){
                    (*it) = std::pair<Symbol,Symbol>(nd->get_name(),feat->get_name());
                    inserted = true;
                }
            }

            if(!inserted){
              meth_list->push_back(std::pair<Symbol,Symbol>(nd->get_name(),feat->get_name()));
            }
        }
    }
}


void CgenClassTable::code_disp_tables(){

    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNodeP nd = l->hd();
        Symbol class_name = nd->get_name();
        emit_disptable_ref(class_name, str);
        str << LABEL;

        std::list<std::pair<Symbol,Symbol> > *meth_list = new std::list<std::pair<Symbol,Symbol> >();

        code_disp_list(nd, meth_list);

        for (std::list<std::pair<Symbol,Symbol> >::iterator it=meth_list->begin(); it != meth_list->end(); ++it){
            str << WORD;
            emit_method_ref((*it).first,(*it).second,str);
            str << endl;
        }
        delete meth_list;
    }
}


void CgenClassTable::code_class_nameTab(){
    str << CLASSNAMETAB << LABEL;

    for(int i = 0; i < number_of_classes; i++){
      Symbol class_name = tags[i];

      str << WORD;
      stringtable.lookup_string(class_name->get_string())->code_ref(str);
      str << endl;
    }
}


void CgenClassTable::code_class_objTab(){
    str << CLASSOBJTAB << LABEL;

    for(int i = 0; i < number_of_classes; i++){
      Symbol class_name = tags[i];

      str << WORD;
      emit_protobj_ref(class_name, str);
      str << endl << WORD;
      emit_init_ref(class_name, str);
      str << endl;
    }

}

int get_attr_size(CgenNodeP nd){
    int attr_n = 0;

    while(nd->get_name() != No_class){
      Features feats = nd->features;
      Feature feat;
      for(int i = feats->first(); feats->more(i); i = feats->next(i)){
        feat = feats->nth(i);
        if(!feat->is_method()){
          attr_n++;
        }
      }

      nd = nd->get_parentnd();
    }
    
    return attr_n;
}


int get_meth_offset(CgenNodeP nd, Symbol meth_name){
    std::list<std::pair<Symbol,Symbol> > *meth_list = new std::list<std::pair<Symbol,Symbol> >();

    code_disp_list(nd, meth_list);

    int offset = 0;

    for (std::list<std::pair<Symbol,Symbol> >::iterator it=meth_list->begin(); it != meth_list->end(); ++it){
        if((*it).second == meth_name){
          break;
        }
    
        offset++;
    }
    
    delete meth_list;
    return offset;

}


////////////////////////////////////////////////


void code_proto_attributes( CgenNodeP nd, ostream& s){
    Symbol class_name = nd->get_name();
    if( class_name == No_class)
      return;

    code_proto_attributes(nd->get_parentnd(), s);

    Features feats = nd->features;
    Feature feat;

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      feat = feats->nth(i);
      if(!feat->is_method()){
        Symbol feat_type = feat->get_type();
        s << WORD;

          if(feat_type == Int){
              inttable.lookup_string("0")->code_ref(s);
          }
          else if (feat_type == Str){
            stringtable.lookup_string("")->code_ref(s);
          }
          else if (feat_type == Bool){
            falsebool.code_ref(s);
          }
          else{
              s << 0;
          }

          s << endl;
      }
    }
}


void CgenClassTable::code_proto_object_def( CgenNodeP nd, ostream& s){

    // Add -1 eye catcher
    s << WORD << "-1" << endl;
    
    Symbol obj_name = nd->get_name();

    emit_protobj_ref(obj_name, s);;  s << LABEL                     // label
        << WORD << class_table->lookup(obj_name)->tag << endl       // class tag
        << WORD << (DEFAULT_OBJFIELDS + get_attr_size(nd)) << endl  // object size
        << WORD; 

        emit_disptable_ref(obj_name,s);  s << endl;                 // dispatch table

        code_proto_attributes(nd,s);
}


void CgenClassTable::code_proto_objects(){
    for(List<CgenNode> *l = nds; l; l = l->tl()){
      code_proto_object_def(l->hd(), str);
    }
}




void attr_class::code(int offset, ostream &s) {
    init->code(s);
    if(!init->is_noexpr()){
      emit_store(ACC, offset, SELF, s);
    }
}


void allocate_stack(int stack_size, ostream &s){
    emit_addiu(SP, SP, - ( WORD_SIZE * stack_size), s);
    emit_store(FP, stack_size--, SP, s);
    emit_store(SELF, stack_size--, SP, s);
    emit_store(RA, stack_size--, SP, s);
}


void deallocate_stack(int stack_size, ostream &s){
    emit_load(FP, stack_size, SP,s);
    emit_load(SELF, stack_size-1, SP,s);
    emit_load(RA, stack_size-2, SP,s);
    
}


void code_initializer(CgenNodeP nd, ostream& s){
  allocate_stack(3, s);
  

  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC,s);

  if (nd->get_name() != Object){
    emit_jal_init(nd->get_parent(),s);
  }

  Features feats = nd->features;

  attribute_table->enterscope();

  build_attribute_table(nd);

  int offset = DEFAULT_OBJFIELDS + get_attr_size(nd->get_parentnd());

  Feature feat;
  for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      feat = feats->nth(i);
      if(!feat->is_method()){
          feat->code(offset++,s);
      }
  }

  attribute_table->exitscope();

  emit_move(ACC, SELF,s);
 
  deallocate_stack(3, s);
  emit_addiu(SP, SP, WORD_SIZE*3, s);
  emit_return(s);
}


void CgenClassTable::code_initializers(){
    CgenNodeP nd;
    for(List<CgenNode> *l = nds; l; l = l->tl()){
      nd = l->hd();
      filename = nd->filename;

      class_table->enterscope();

      class_table->addid(SELF_TYPE, new obj_elem{-1,nd});

      emit_init_ref(nd->get_name(),str);
      str << LABEL;
      code_initializer(nd, str);
      class_table->exitscope();
    }
}


void method_class::code(int dummy, ostream &s) {
    
    int stack_size = 3 + expr->count_stack(0);

    allocate_stack(stack_size,s);

    emit_addiu(FP, SP, 4, s);
    emit_move(SELF, ACC,s);

    
    attribute_table->enterscope();

    int formal_length = 0;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)){
        formal_length++;
    }

    int number_of_parameters = formal_length;
    Formal formal;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)){
        formal = formals->nth(i);
        formal_length--;
        attribute_table->addid(formal->get_name(), new attr_elem{formal->get_type(),formal_length + stack_size,FP});
    }

    expr->code(s);

    attribute_table->exitscope();

    
    deallocate_stack(stack_size,s);
    emit_addiu(SP, SP, WORD_SIZE*(stack_size+number_of_parameters), s);
    emit_return(s);
}


void CgenClassTable::code_methods(){

    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNodeP nd = l->hd();
        
        filename = nd->filename;

        attribute_table->enterscope();

        build_attribute_table(nd);

        class_table->enterscope();

        class_table->addid(SELF_TYPE, new obj_elem{-1,nd});
        
        if(!nd->basic()){
          Features feats = nd->features;
          Feature feat;
          for(int i = feats->first(); feats->more(i); i = feats->next(i)){
              feat = feats->nth(i);
              if(feat->is_method()){
                  emit_method_ref(nd->get_name(), feat->get_name(), str); str << LABEL;
                      feat->code(0,str);
              }
          }  
        }
        class_table->exitscope();
        attribute_table->exitscope();
    }

}


int set_new_label(){
  return label_index++;
}


void get_biggest_son_tag_helper(CgenNodeP nd, int *tag){
    
    (*tag)++;
    for(List<CgenNode> *l = nd->get_children(); l; l = l->tl()){
      get_biggest_son_tag_helper(l->hd(),tag);
    }
}

int get_biggest_son_tag(CgenNodeP nd){
    int tag = class_table->lookup(nd->get_name())->tag;
    for(List<CgenNode> *l = nd->get_children(); l; l = l->tl()){
      get_biggest_son_tag_helper(l->hd(),&tag);
    }
    return tag;
}



void CgenClassTable::code()
{

  attribute_table = new SymbolTable<Symbol, attr_elem> ();

  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//



  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_disp_tables();

  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_proto_objects();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  if (cgen_debug) cout << "coding initializers" << endl;
  code_initializers();

  if (cgen_debug) cout << "coding methods" << endl;
  code_methods();

  delete attribute_table;
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}



//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void branch_class::code(ostream &s){
  s << "# Start of Branch" << endl;
  attribute_table->enterscope();
  attribute_table->addid(name, new attr_elem{type_decl,offset,FP});

  obj_elem *obj = class_table->lookup(type_decl);

  CgenNodeP nd = obj->class_node;
  int smallest_tag = obj->tag;
  int biggest_tag = get_biggest_son_tag(nd);


  int next_label = set_new_label();

  emit_blti(T2,smallest_tag,next_label,s);
  emit_bgti(T2,biggest_tag,next_label,s);


  emit_store(ACC,offset,FP,s);

  expr->code(s);
  attribute_table->exitscope();

  emit_branch(end_label,s);
  emit_label_ref(next_label,s);
  s << LABEL;
  s << "# End of Branch" << endl;
}


void assign_class::code(ostream &s) {
  s << "# Start of assign" << endl;
  expr->code(s);

  attr_elem *obj = (attribute_table->lookup(name));

  emit_store(ACC, obj->offset,obj->reg, s);
  s << "# End of assign" << endl;

}

void static_dispatch_class::code(ostream &s) {
  s << "# Start of static_dispatch" << endl;

  //Stack arguments
   for(int i = actual->first(); actual->more(i); i = actual->next(i)){
      actual->nth(i)->code(s);
      emit_push(ACC,s);
  }
  ////////////////


  expr->code(s);
  
  int disp_label = set_new_label();

  emit_bne(ACC,ZERO,disp_label, s);         //Test for dispatch on void

  /////// Dispatch on void ///////////////
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(filename->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1,this->get_line_number(),s);
  emit_jal("_dispatch_abort",s);
  ////////////////////////////////////////

  emit_label_def(disp_label, s);

  emit_partial_load_address(T1, s);
  emit_disptable_ref(type_name, s);
  s << endl;

  obj_elem *obj_class = (class_table->lookup(type_name));
  CgenNodeP nd = obj_class->class_node;

  int meth_offset = get_meth_offset(nd,name);

  emit_load(T1,meth_offset, T1, s);

  emit_jalr(T1,s);

  s << "# End of static_dispatch" << endl;
  
}

void dispatch_class::code(ostream &s) {
  s << "# Start of dispatch" << endl;

  //Stack arguments
  for(int i = actual->first(); actual->more(i); i = actual->next(i)){
      actual->nth(i)->code(s);
      emit_push(ACC,s);
  }

  expr->code(s);
  
  int disp_label = set_new_label();

  emit_bne(ACC,ZERO,disp_label, s);         //Test for dispatch on void


  /////// Dispatch on void ///////////////
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(filename->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1,this->get_line_number(),s);
  emit_jal("_dispatch_abort",s);
  ////////////////////////////////////////

  emit_label_def(disp_label, s);

  emit_load(T1, DISPTABLE_OFFSET, ACC, s);    //Get disptable

  obj_elem *obj_class = (class_table->lookup(expr->get_type()));
  CgenNodeP nd = obj_class->class_node;

  int meth_offset = get_meth_offset(nd,name);

  emit_load(T1,meth_offset, T1, s);

  emit_jalr(T1,s);

  s << "# End of dispatch" << endl;

}

void cond_class::code(ostream &s) {
  int else_label, end_label;
  s << "# Start of condition" << endl;

  pred->code(s);

  else_label = set_new_label();

  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s); //Get value of Bool obj predicate
  emit_beqz(T1, else_label, s);             //Checks if value is zero

  then_exp->code(s);                        //Generates then branch

  end_label = set_new_label();
  emit_branch(end_label,s);                 //Goes to end

  emit_label_def(else_label, s);
  else_exp->code(s);                        //Generates else branch
  emit_label_def(end_label, s);

  s << "# End of condition" << endl;
}

void loop_class::code(ostream &s) {
  int loop_label, end_label;
  s << "# Start of loop" << endl;
  
  loop_label = set_new_label();

  emit_label_def(loop_label,s);

  pred->code(s);

  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s); //Get value of Bool objs

  end_label = set_new_label();
  emit_beqz(T1, end_label, s);              //Checks if value is zero
  
  body->code(s);

  emit_branch(loop_label, s);

  emit_label_def(end_label, s);
  emit_move(ACC,ZERO,s);
  s << "# End of loop" << endl;
}

void typcase_class::code(ostream &s) {
  s << "# Start of case" << endl;


  expr->code(s);

  int end_label = set_new_label();
  int case_label = set_new_label();


  emit_bne(ACC,ZERO,case_label, s);         //Test for dispatch on void

  /////// case on void ///////////////
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(filename->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1,this->get_line_number(),s);
  emit_jal("_case_abort2",s);
  ////////////////////////////////////////

  emit_label_def(case_label, s);

  emit_load(T2,0,ACC,s);        //Get Tag

  Case branch;
  for(int i = cases->first(); cases->more(i); i = cases->next(i)){
      branch = cases->nth(i);
      branch->set_label(end_label);
      branch->code(s);
  }

  //no match
  emit_jal("_case_abort",s);


  emit_label_def(end_label, s);

  s << "# End of case" << endl;
}

void block_class::code(ostream &s) {
  for(int i = body->first(); body->more(i); i = body->next(i)){
      body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
  s << "# Start of let" << endl;
  attribute_table->enterscope();

  attribute_table->addid(identifier, new attr_elem{type_decl,offset,FP});

  if(init->is_noexpr()){
    emit_partial_load_address(ACC, s);
    if(type_decl == Int){
        inttable.lookup_string("0")->code_ref(s);
    }
    else if (type_decl == Str){
      stringtable.lookup_string("")->code_ref(s);
    }
    else if (type_decl == Bool){
      falsebool.code_ref(s);
    }
    else{
        s << 0;
    }
    s << endl;
  }
  else{
    init->code(s);
  }

  emit_store(ACC, offset, FP, s);

  body->code(s);
  attribute_table->exitscope();


  s << "# End of let" << endl;
}

void plus_class::code(ostream &s) {
  s << "# Start of plus" << endl;
  e1->code(s);
  emit_push(ACC,s);                           //Saves first int object
  e2->code(s);
  emit_jal("Object.copy",s);                  //Allocates new object on heap and stores its address on acc
  emit_pop(T1,s);                             //Puts first int object on temporary register

  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);    //Gets attribute val of int object
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);   //Gets attribute val of int object
  emit_add(T1,T1,T2,s);                       //Adds values
  
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);  //Store result on allocated object
  s << "# End of plus" << endl;
  
}

void sub_class::code(ostream &s) {
  s << "# Start of sub" << endl;
  e1->code(s);
  emit_push(ACC,s);                           //Saves first int object
  e2->code(s);
  emit_jal("Object.copy",s);                  //Allocates new object on heap and stores its address on acc
  emit_pop(T1,s);                             //Puts first int object on temporary register

  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);    //Gets attribute val of int object
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);   //Gets attribute val of int object
  emit_sub(T1,T1,T2,s);                       //Adds values
  
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);  //Store result on allocated object
  
  s << "# End of sub" << endl;
}

void mul_class::code(ostream &s) {
  s << "# Start of mul" << endl;
  e1->code(s);
  emit_push(ACC,s);                           //Saves first int object
  e2->code(s);
  emit_jal("Object.copy",s);                  //Allocates new object on heap and stores its address on acc
  emit_pop(T1,s);                             //Puts first int object on temporary register

  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);    //Gets attribute val of int object
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);   //Gets attribute val of int object
  emit_mul(T1,T1,T2,s);                       //Adds values
  
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);  //Store result on allocated object
  
  s << "# End of mul" << endl;
}

void divide_class::code(ostream &s) {
  s << "# Start of div" << endl;
  e1->code(s);
  emit_push(ACC,s);                           //Saves first int object
  e2->code(s);
  emit_jal("Object.copy",s);                  //Allocates new object on heap and stores its address on acc
  emit_pop(T1,s);                             //Puts first int object on temporary register

  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);    //Gets attribute val of int object
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);   //Gets attribute val of int object
  emit_div(T1,T1,T2,s);                       //Adds values
  
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);  //Store result on allocated object
  
  s << "# End of div" << endl;
}

void neg_class::code(ostream &s) {
  s << "# Start of neg" << endl;
  
  e1->code(s);
  emit_jal("Object.copy",s);                  //Allocates new object on heap and stores its address on acc
  
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);   //Gets attribute val of int object
  emit_neg(T1,T1,s);                          //Negates value
  
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);  //Store result on allocated object
  
  s << "# End of neg" << endl;
}

void lt_class::code(ostream &s) {
  s << "# Start of lt" << endl;
  e1->code(s);
  emit_push(ACC,s);                         //Saves first int object
  e2->code(s);
  emit_pop(T1, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s); //Get value of first Int obj
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s); //Get value of second Int obj

  
  emit_load_bool(ACC,BoolConst(1), s);      //Sets True as the result

  int end_label = set_new_label();
  emit_blt(T1, T2, end_label, s);           //Checks if first value is < than seconf
  emit_load_bool(ACC,BoolConst(0), s);      //If value was not < sets False as complement
  emit_label_def(end_label, s);
  s << "# End of lt" << endl;
}

void eq_class::code(ostream &s) {
  s << "# Start of eq" << endl;
  e1->code(s);
  emit_push(ACC,s);                         //Saves first int object
  e2->code(s);
  emit_pop(T1, s);
  emit_move(T2, ACC, s);                    //Get second obj
  
  emit_load_bool(ACC,BoolConst(1), s);      //Sets True as the result

  int end_label = set_new_label();
  emit_beq(T1, T2, end_label, s);           //Check if both objects are the same (same address)

  emit_load_bool(A1,BoolConst(0), s);       //If objects are not the same prepare false result
  emit_jal("equality_test", s);             //Calls function equality_test
  emit_label_def(end_label, s);
  s << "# End of eq" << endl;
}

void leq_class::code(ostream &s) {
  s << "# Start of leq" << endl;
  e1->code(s);
  emit_push(ACC,s);                         //Saves first int object
  e2->code(s);
  emit_pop(T1, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s); //Get value of first Int obj
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s); //Get value of second Int obj
  emit_load_bool(ACC,BoolConst(1), s);      //Sets True as the result

  int end_label = set_new_label();
  emit_bleq(T1, T2, end_label, s);          //Checks if first value is <= than seconf
  emit_load_bool(ACC,BoolConst(0), s);      //If value was not <= sets False as complement
  emit_label_def(end_label, s);
  s << "# End of leq" << endl;
}

void comp_class::code(ostream &s) {
  s << "# Start of not" << endl;
  e1->code(s);
  
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s); //Get value of Bool objs
  emit_load_bool(ACC,BoolConst(1), s);      //Sets True as the complement

  int end_label = set_new_label();
  emit_beqz(T1, end_label, s);              //Checks if value is zero
  emit_load_bool(ACC,BoolConst(0), s);      //If value was not zero sets False as complement
  emit_label_def(end_label, s);


  s << "# End of not" << endl;  
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  s << "# Start of new" << endl;
  if (type_name == SELF_TYPE){
      emit_load_address(T1, CLASSOBJTAB, s);  //Get adress of ObjTab
      emit_load(T2, 0, SELF, s);              //Get class tag
      emit_sll(T2, T2, 3, s);                 //Get relative location of class protoObj from the start of ObjTab (tag*8)
      emit_addu(T1, T1, T2, s);               //Get location of protoObj
      emit_push(T1,s);                        //Store location of protoObj
      emit_load(ACC,0,T1,s);                  //Puts location of protoObj on accumulator
      emit_jal("Object.copy",s);              //Allocates new object on heap     
      emit_pop(T1,s);                         //Retrieves location of protoObj
      emit_addiu(T1, T1, 4, s);               //Gets location of init (Following position to protoObj on ObjTab)
      emit_jalr(T1, s);                       //Jumps to class init
  }
  else{
    emit_partial_load_address(ACC,s);
    emit_protobj_ref(type_name, s);
    s << endl;

    emit_jal("Object.copy",s);

    emit_jal_init(type_name,s);
  }
  s << "# End of new" << endl;

}

void isvoid_class::code(ostream &s) {
  s << "# Start of isvoid" << endl;
  e1->code(s);
  emit_move(T1, ACC, s);                  //Saves value of expression
  emit_load_bool(ACC,BoolConst(1), s);    //Sets True as answer

  int end_label = set_new_label();
  emit_beqz(T1, end_label, s);           //Checks if object is 0 (void)
  emit_load_bool(ACC,BoolConst(0), s);    //If object is not void sets False as answer 
  emit_label_def(end_label, s);
  s << "# End of isvoid" << endl;
}

void no_expr_class::code(ostream &s) {
}

void object_class::code(ostream &s) {
  s << "# Start of object" << endl;

  if(name == self){
    emit_move(ACC, SELF, s);
  }
  else{
    attr_elem *obj = (attribute_table->lookup(name));

    emit_load(ACC, obj->offset, obj->reg, s);

  }
  
  s << "# End of object" << endl;

}



int branch_class::count_stack(int depth){
  offset = depth;

  return 1 + expr->count_stack(depth+1);
}


int assign_class::count_stack(int depth) {
  return expr->count_stack(depth);
}

int static_dispatch_class::count_stack(int depth) {
  int expr_c, actual_c, c;
  
  expr_c = expr->count_stack(depth);
  actual_c = 0;

  for(int i = actual->first(); actual->more(i); i = actual->next(i)){
      c = actual->nth(i)->count_stack(depth);
      if (c > actual_c){
        actual_c = c;
      }

  }

  return (expr_c > actual_c) ? expr_c : actual_c;
}

int dispatch_class::count_stack(int depth) {
  int expr_c, actual_c, c;
  
  expr_c = expr->count_stack(depth);
  actual_c = 0;

  for(int i = actual->first(); actual->more(i); i = actual->next(i)){
      c = actual->nth(i)->count_stack(depth);
      if (c > actual_c){
        actual_c = c;
      }

  }

  return (expr_c > actual_c) ? expr_c : actual_c;
}

int cond_class::count_stack(int depth) {
  int pred_c, then_c, else_c;

  pred_c = pred->count_stack(depth);
  then_c = then_exp->count_stack(depth);
  else_c = else_exp->count_stack(depth);
  return (pred_c > then_c) ?  ( (pred_c > else_c) ? pred_c : else_c) :
                              ( (then_c > else_c) ? then_c : else_c);
}

int loop_class::count_stack(int depth) {
  int pred_c, body_c;

  pred_c = pred->count_stack(depth);
  body_c = body->count_stack(depth);

  return (pred_c > body_c) ? pred_c : body_c;
}

int typcase_class::count_stack(int depth) {
  int expr_c, count;
  
  count = 0;
  
  for(int i = cases->first(); cases->more(i); i = cases->next(i)){
      int c = cases->nth(i)->count_stack(depth);
      if (c > count){
        count = c;
      }
  }
  
  expr_c = expr->count_stack(depth);
  
  return (count > expr_c) ? count : expr_c;
}

int block_class::count_stack(int depth) {
  int c, count = 0;

  for(int i = body->first(); body->more(i); i = body->next(i)){
      c = body->nth(i)->count_stack(depth);
      if (c > count){
        count = c;
      }
  }

  return count;
}

int let_class::count_stack(int depth) {
  int init_c, body_c;
  
  offset = depth;
  init_c = init->count_stack(depth+1);
  body_c = body->count_stack(depth+1);

  return 1 + ((init_c > body_c) ? init_c : body_c);
}

int plus_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int sub_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int mul_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int divide_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int neg_class::count_stack(int depth) {
  return e1->count_stack(depth);
}

int lt_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int eq_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int leq_class::count_stack(int depth) {
  int e1_c = e1->count_stack(depth);
  int e2_c = e2->count_stack(depth);
  return (e1_c > e2_c) ? e1_c : e2_c;
}

int comp_class::count_stack(int depth) {
  return e1->count_stack(depth);
}

int int_const_class::count_stack(int depth) {
  return 0;
}

int string_const_class::count_stack(int depth){
  return 0;
}

int bool_const_class::count_stack(int depth){
  return 0;
}

int new__class::count_stack(int depth) {
  return 0;
}

int isvoid_class::count_stack(int depth) {
  return e1->count_stack(depth);
}

int no_expr_class::count_stack(int depth) {
  return 0;
}

int object_class::count_stack(int depth) {
  return 0;
}