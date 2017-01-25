
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
{ s << JAL; emit_init_ref(sym, s); s<< endl; }
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
   stringclasstag = 4 /* Change to your String class tag here */;
   intclasstag =    2 /* Change to your Int class tag here */;
   boolclasstag =   3 /* Change to your Bool class tag here */;

   label_index = 0;
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   set_tags();
   code();
   exitscope();
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
    cout << class_name << endl;
    if( class_name == No_class){
      attribute_table->addid(self, new std::pair<int, char*>(0,SELF));
      return 0;
    }

    int attr_n = build_attribute_table(nd->get_parentnd());

    Features feats = nd->features;

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      Feature feat = feats->nth(i);
      if(!feat->is_method()){
        attribute_table->addid(feat->get_name(), new std::pair<int, char*>(attr_n + DEFAULT_OBJFIELDS,SELF));
        attr_n++;
      }
    }

    return attr_n;
}



void CgenClassTable::set_tags(){
    int i = number_of_classes - 1;
    tags = new Symbol[number_of_classes];
     for(List<CgenNode> *l = nds; l && (i >= 0); l = l->tl(), i--){
      tags[i] =  l->hd()->get_name();
    }
}

int CgenClassTable::get_tag(Symbol name){
    for(int i = 0; i < number_of_classes; i++){
      if(tags[i] == name){
        return i;
      }
    }
    return number_of_classes;
}

bool is_lowest_method(Features feats, Symbol method_name){
    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
        Feature feat = feats->nth(i);
        if( feat->is_method() && (feat->get_name() == method_name)){
          return false;
        }
    }
    return true;

}


void code_disp_table(CgenNodeP nd, Features past_feats, ostream& s){
    Symbol class_name = nd->get_name();
    if( class_name == No_class)
      return;
    
    Features feats = nd->features;

    code_disp_table(nd->get_parentnd(), append_Features(past_feats,feats), s);

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      Feature feat = feats->nth(i);
      if(feat->is_method() && is_lowest_method(past_feats,feat->get_name())){
        s << WORD;
          emit_method_ref(class_name,feat->get_name(),s);
          s << endl;
      }
    }

}


void CgenClassTable::code_disp_tables(){

    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNodeP nd = l->hd();
        Symbol class_name = nd->get_name();
        emit_disptable_ref(class_name, str);
        str << LABEL;
        code_disp_table(nd, nil_Features(), str);
    }

}


void code_class_nameTab_help(List<CgenNode> *l, ostream& s){
      Symbol class_name = l->hd()->get_name();
      if(class_name != Object){
          code_class_nameTab_help(l->tl(),s);
      }

      s << WORD;
      stringtable.lookup_string(class_name->get_string())->code_ref(s);
      s << endl;
}


void CgenClassTable::code_class_nameTab(){
    str << CLASSNAMETAB << LABEL;

    code_class_nameTab_help(nds,str);
}


void code_class_objTab_help(List<CgenNode> *l, ostream& s){

      Symbol class_name = l->hd()->get_name();

      if(class_name != Object){
          code_class_objTab_help(l->tl(),s);
      }

      s << WORD;
      emit_protobj_ref(class_name, s);
      s << endl << WORD;
      emit_init_ref(class_name, s);
      s << endl;

}

void CgenClassTable::code_class_objTab(){
    str << CLASSOBJTAB << LABEL;

    code_class_objTab_help(nds,str);
}



int get_attr_size(CgenNodeP nd){

   Symbol class_name = nd->get_name();
    if( class_name == No_class)
      return 0;

    int attr_n = 0;

    Features feats = nd->features;

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      Feature feat = feats->nth(i);
      if(!feat->is_method()){
        attr_n++;
      }
    }

    return attr_n + get_attr_size(nd->get_parentnd());
}


int get_meth_size(CgenNodeP nd){

   Symbol class_name = nd->get_name();
    if( class_name == No_class)
      return 0;

    int meth_n = 0;

    Features feats = nd->features;

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      Feature feat = feats->nth(i);
      if(feat->is_method()){
        meth_n++;
      }
    }

    return meth_n + get_meth_size(nd->get_parentnd());
}


void code_proto_attributes( CgenNodeP nd, ostream& s){
    Symbol class_name = nd->get_name();
    if( class_name == No_class)
      return;

    code_proto_attributes(nd->get_parentnd(), s);

    Features feats = nd->features;

    for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      Feature feat = feats->nth(i);
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

    emit_protobj_ref(obj_name, s);;  s << LABEL                  // label
        << WORD << get_tag(obj_name) << endl                        // class tag
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


void code_initializer(CgenNodeP nd, ostream& s){
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC,s);

  if (nd->get_name() != Object){
    emit_jal_init(nd->get_parent(),s);
  }

  Features feats = nd->features;

  attribute_table->enterscope();

  build_attribute_table(nd);

  int offset = DEFAULT_OBJFIELDS + get_attr_size(nd->get_parentnd());

  for(int i = feats->first(); feats->more(i); i = feats->next(i)){
      Feature feat = feats->nth(i);
      if(!feat->is_method()){
          feat->code(offset++,s);
      }
  }

  attribute_table->exitscope();

  emit_move(ACC, SELF,s);
  emit_load(FP, 3, SP,s);
  emit_load(SELF, 2, SP,s);
  emit_load(RA, 1, SP,s);
  emit_addiu(SP, SP, 12, s);
  emit_return(s);
}


void CgenClassTable::code_initializers(){
    for(List<CgenNode> *l = nds; l; l = l->tl()){
      emit_init_ref(l->hd()->get_name(),str);
      str << LABEL;
      code_initializer(l->hd(), str);
    }
}




void method_class::code(int offset, ostream &s) {
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);
    emit_move(SELF, ACC,s);
    cout << 1 << endl;
    
    attribute_table->enterscope();
    cout << 2 << endl;
    int formal_length = 0;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)){
        formal_length++;
    }
cout << 3 << endl;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)){
        Formal formal = formals->nth(i);
        attribute_table->addid(formal->get_name(), new std::pair<int, char*>(formal_length + 2,FP));
        formal_length--;
    }
cout << 4 << endl;

    expr->code(s);
cout << 5 << endl;
    attribute_table->exitscope();
cout << 6 << endl;
    emit_load(FP, 3, SP,s);
    emit_load(SELF, 2, SP,s);
    emit_load(RA, 1, SP,s);
    emit_addiu(SP, SP, 12, s);
    emit_return(s);
}


void CgenClassTable::code_methods(){

    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNodeP nd = l->hd();
        
        attribute_table->enterscope();

        build_attribute_table(nd);

  

        if(!nd->basic()){
          Features feats = nd->features;
          int offset = get_meth_size(nd->get_parentnd());

          for(int i = feats->first(); feats->more(i); i = feats->next(i)){
              Feature feat = feats->nth(i);
              if(feat->is_method()){
                  emit_method_ref(nd->get_name(), feat->get_name(), str); str << LABEL;
                      feat->code(offset++,str);
              }
          }  
        }
        attribute_table->exitscope();
    }

}


int set_new_label(){
  return label_index++;
}


void CgenClassTable::code()
{

  attribute_table = new SymbolTable<Symbol, std::pair<int, char*> > ();
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

void assign_class::code(ostream &s) {
  expr->code(s);
  s << "assign" << endl;
}

void static_dispatch_class::code(ostream &s) {
  s << "static_dispatch" << endl;
}

void dispatch_class::code(ostream &s) {
  s << "dispatch" << endl;
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
  s << "typcase" << endl;
}

void block_class::code(ostream &s) {
  for(int i = body->first(); body->more(i); i = body->next(i)){
      body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
  s << "let" << endl;
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
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s); //Get value of first Int obj
  e2->code(s);
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
  emit_move(T1, ACC, s);                    //Get first obj
  e2->code(s);
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
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s); //Get value of first Int obj
  e2->code(s);
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
      emit_pop(T1,s);                         //Retrives location of protoObj
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
  emit_beqz(ACC, end_label, s);           //Checks if object is 0 (void)
  emit_load_bool(ACC,BoolConst(0), s);    //If object is not void sets False as answer 
  emit_label_def(end_label, s);
  s << "# End of isvoid" << endl;
}

void no_expr_class::code(ostream &s) {
}

void object_class::code(ostream &s) {
  s << "# Start of object" << endl;
  cout << 11<<endl;
  std::pair<int, char*> obj = *(attribute_table->lookup(name));
  cout << 12<<endl;
  int offset = obj.first;
  cout << 13<<endl;
  char* reg = obj.second;
  cout << 14<<endl;
  if (offset != 0){
    emit_load(ACC, offset, reg, s);
  }
  else{
    emit_move(ACC, reg, s);
  }
  cout << 15<<endl;
  s << "# End of object" << endl;


}


