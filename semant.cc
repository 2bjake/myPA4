

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <set>
#include <vector>
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

void ClassTable::add_class(Class_ c) {
    class_symbols.insert(c->get_name());
    tbl->addid(c->get_name(), c);
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    tbl = new SymbolTable<Symbol, Class__class>();

    tbl->enterscope();

    // add user defined classes to symbol table
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);

        // check for redefinition of built-in classes
        Symbol name = c->get_name();
        if (name == Object || name == Int  || name == Bool || name == Str || name == IO) {
            semant_error(c) << "Class " << name << " is a built-in class and cannot be redefined" << std::endl;
            continue;
        }

        // check for improper use of SELF_TYPE
        if (name == SELF_TYPE) {
            semant_error(c) << "SELF_TYPE cannot be used as a class name" << std::endl;
            continue;
        }

        Symbol parent_sym = c->get_parent_sym();

        // check for self inheritance
        if (parent_sym == name || parent_sym == SELF_TYPE) {
            semant_error(c) << "Class " << name << " cannot inherit from itself" << std::endl;
            continue;
        }

        // check for inheritance of invalid built-in classes
        if (parent_sym == Int  || parent_sym == Bool || parent_sym == Str) {
            semant_error(c) << "Class " << name << " cannot inherit from " << parent_sym << std::endl;
            continue;
        }

        // check for redefinition of user classes
        if (tbl->lookup(name) != NULL) {
            semant_error(c) << "Class " << name << " already defined" << std::endl;
            continue;
        }

        // didn't continue (no errors) add to symbol table
        add_class(c);
    }

    // check that Main class is defined
    if (tbl->lookup(Main) == NULL) {
        semant_error() << "Required class 'Main' not defined" << std::endl;
    }

    install_basic_classes();

    // check inheritance
    for(std::set<Symbol>::iterator i = class_symbols.begin(); i != class_symbols.end(); i++) {
        Class_ c = tbl->lookup(*i);
        if (c == NULL) {
            continue; // only process classes that made it into the symbol table
        }

        if (c->get_parent_sym() != No_class && tbl->lookup(c->get_parent_sym()) == NULL) {
            semant_error(c) << "Class " << c->get_name() << " cannot inherit from " << c->get_parent_sym() << " because " << c->get_parent_sym() << " is not defined" << std::endl;
            continue;
        }

        // check for inheritance cycles
        bool has_cycle = false;
        Class_ cur = tbl->lookup(c->get_parent_sym());
        while (!has_cycle && cur != NULL) {
            if (cur->get_name() == c->get_name()) {
                semant_error(c) << "Class " << c->get_name() << " has an inheritance cycle" << std::endl;
                has_cycle = true;
            }
            cur = tbl->lookup(cur->get_parent_sym());
        }

        // if inheritance checks out, add c to parent's children
        Class_ parent = class_for_symbol(c->get_parent_sym());
        if (!has_cycle && parent != NULL) {
            parent->add_child(c->get_name());
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
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

    add_class(Object_class);

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

    add_class(IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    add_class(Int_class);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    add_class(Bool_class);
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

    add_class(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
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

bool program_class::check_add_attr(attr_class* attr, Class_ c, ClassTable* classtable) {
    // check that the type is in classSymbols (or is SELF_TYPE)
    if (classtable->class_for_symbol(attr->get_type_decl()) == NULL && attr->get_type_decl() != SELF_TYPE) {
        classtable->semant_error(c->get_filename(), attr) << "Attribute " << attr->get_name() << " type " << attr->get_type_decl() << " is undefined" << std::endl;
        return false;
    }

    // check that the id is not self
    if (attr->get_name() == self) {
        classtable->semant_error(c->get_filename(), attr) << "self cannot be used as an attribute name" << std::endl;
        return false;
    }

    // check that the id is not already in the attrTable
    if (attr_tbl->lookup(attr->get_name()) != NULL) {
        classtable->semant_error(c->get_filename(), attr) << attr->get_name() << " cannot be redefined" << std::endl;
        return false;
    }

    attr_tbl->addid(attr->get_name(), attr->get_type_decl());
    return true;
}

bool program_class::check_add_method(method_class* method, Class_ c, ClassTable* classtable) {
    // check that the return type is in classSymbols (or is SELF_TYPE)
    if (classtable->class_for_symbol(method->get_return_type()) == NULL && method->get_return_type() != SELF_TYPE) {
        classtable->semant_error(c->get_filename(), method) << "Method " << method->get_name() << " return type " << method->get_return_type() << " is undefined" << std::endl;
        return false;
    }

    // check that id is not self
    if (method->get_name() == self) {
        classtable->semant_error(c->get_filename(), method) << "self cannot be used as a method name" << std::endl;
        return false;
    }

    // formals checks
    std::set<Symbol> formal_names;
    Formals formals = method->get_formals();
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal f = formals->nth(i);
        // check that types are in classSymbols (SELF_TYPE not allowed)
        if (classtable->class_for_symbol(f->get_type()) == NULL) {
            classtable->semant_error(c->get_filename(), method) << "Method " << method->get_name() << " parameter " << f->get_name() << " has undefined type " << f->get_type() << std::endl;
            return false;
        }

        // check that id is not self
        if (f->get_name() == self) {
            classtable->semant_error(c->get_filename(), method) << "Cannot use self as parameter name in method " << method->get_name() << std::endl;
            return false;
        }

        // check that the same id isn't used multiple times
        std::pair<std::set<Symbol>::iterator, bool> result = formal_names.insert(f->get_name());
        if (!result.second) {
            classtable->semant_error(c->get_filename(), method) << "Cannot use the same paramater name " << f->get_name() << " more than once in method " << method->get_name() << std::endl;
            return false;
        }
    }

    // check that if id is already in methodTable, that the type matches exactly (including SELF_TYPE return)
    if (method_class* super_method = method_tbl->lookup(method->get_name())) {
        if (method->get_return_type() != super_method->get_return_type()) {
            classtable->semant_error(c->get_filename(), method) << "In redefined method " << method->get_name() << ", return type " << method->get_return_type() << " is different from original type " << super_method->get_return_type() << std::endl;
            return false;
        }

        if (method->get_formals()->len() != super_method->get_formals()->len()) {
            classtable->semant_error(c->get_filename(), method) << "In redefined method " << method->get_name() << ", the wrong number of parameters are specified" << std::endl;
            return false;
        }

        for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Symbol type = formals->nth(i)->get_type();
            Symbol super_type = super_method->get_formals()->nth(i)->get_type();
            if (type != super_type) {
                classtable->semant_error(c->get_filename(), method) << "In redefined method " << method->get_name() << ", parameter type " << type << " does not match original parameter type " << super_type << std::endl;
                return false;
            }
        }
    }
    method_tbl->addid(method->get_name(), method);
    return true;
}

void program_class::process_class(Symbol cur_sym, ClassTable* classtable) {
    Class_ cur = classtable->class_for_symbol(cur_sym);
    attr_tbl->enterscope();
    method_tbl->enterscope();
    Features features = cur->get_features();
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (attr_class* attr = dynamic_cast<attr_class*>(f)) {
            check_add_attr(attr, cur, classtable);
        } else if (method_class* method = dynamic_cast<method_class*>(f)) {
            check_add_method(method, cur, classtable);
        }
    }

    // check if Main class has a main method
    if (cur_sym == Main && method_tbl->probe(main_meth) == NULL) {
        classtable->semant_error(classtable->class_for_symbol(cur_sym)) << "class Main must have a main method" << std::endl;
    }

    // all of this class' valid attributes and methods are now in the table
    // if this is not built-in class, go back through features and evaluate expressions and type check
    if (cur_sym != Object && cur_sym != IO && cur_sym != Int && cur_sym != Str && cur_sym != Bool) {
        attr_tbl->enterscope();
        attr_tbl->addid(self, cur_sym);
        for(int i = features->first(); features->more(i); i = features->next(i)) {
            features->nth(i)->typecheck(cur, classtable, attr_tbl); // TODO: problem, check_add_method needs to be called for all classes before we can start typechecking any classes....
        }
        attr_tbl->exitscope();
    }

    // process children classes
    std::set<Symbol> children = cur->get_children();
    for(std::set<Symbol>::iterator i = children.begin(); i != children.end(); i++ ) {
        if (*i != Bool && *i != Int && *i != Str) {
            process_class(*i, classtable);
        }
    }

    attr_tbl->exitscope();
    method_tbl->exitscope();
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
    ClassTable *classtable = new ClassTable(classes);

    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }

    process_class(Object, classtable);

    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }
}

void class__class::index_methods() {
      for(int i = features->first(); features->more(i); i = features->next(i)) {
         Feature f = features->nth(i);
         if (method_class* method = dynamic_cast<method_class*>(f)) {
            methods[method->get_name()] = method;
         }
      }
}

bool ClassTable::conforms(Symbol c, Symbol super) {
    Class_ cur = class_for_symbol(c);
    while (cur != NULL) {
        if (cur->get_name() == super) {
            return true;
        }
        cur = class_for_symbol(cur->get_parent_sym());
    }
    return false;
}

Symbol true_type(Class_ c, Symbol type) {
    return (type == SELF_TYPE) ? c->get_name() : type;
}

bool class__class::has_method_of_type(Symbol name, Symbol return_type, std::vector<Symbol> param_types, ClassTable* classtable) {
    if (method_class* method = methods[name]) {
        if (return_type != true_type(this, method->get_return_type())) {
            return false;
        }

        Formals formals = method->get_formals();
        if ((size_t)(formals->len()) != param_types.size()) {
            return false;
        }
        for (int i = 0; i < formals->len(); i++) {
            if (formals->nth(i)->get_type() != true_type(this, param_types[i])) {
                return false;
            }
        }
        return true;
    } else if (Class_ p = classtable->class_for_symbol(parent)) {
        return p->has_method_of_type(name, return_type, param_types, classtable);
    } else {
        return false;
    }
}

Symbol ClassTable::least_type(Symbol a, Symbol b) {
    std::vector<Symbol> aPath;
    Class_ cur = class_for_symbol(a);
    while (cur != NULL) {
        aPath.push_back(cur->get_name());
        cur = class_for_symbol(cur->get_parent_sym());
    }
    int ai = aPath.size() - 1;

    std::vector<Symbol> bPath;
    cur = class_for_symbol(b);
    while (cur != NULL) {
        bPath.push_back(cur->get_name());
        cur = class_for_symbol(cur->get_parent_sym());
    }
    int bi = bPath.size() - 1;

    while (ai >= 0 && bi >= 0) {
        if (aPath[ai] != bPath[bi]) {
            break;
        }
        ai--;
        bi--;
    }
    return aPath[ai + 1];
}

// typechecker functions

void attr_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    if (init->has_expression()) {
        Symbol type = type_decl == SELF_TYPE ? c->get_name() : type_decl;
        bool init_success = init->typecheck(c, classtable, attr_tbl);
        if (init_success && !classtable->conforms(init->get_type(), type)) {
            classtable->semant_error(c->get_filename(), init) << "Initialization type " << init->get_type() << " for attribute " << name << " does not conform to type " << type_decl << std::endl;
        }
    }
}


void method_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    // TODO
}

// contants

bool int_const_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Int;
    return true;
}

bool bool_const_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Bool;
    return true;
}

bool string_const_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Str;
    return true;
}

// binary operators

bool binary_op_typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl, Expression e1, Expression e2, Symbol type, Symbol op) {
    bool e1_success = e1->typecheck(c, classtable, attr_tbl);
    bool e2_success = e2->typecheck(c, classtable, attr_tbl);
    if (!e1_success || !e2_success) {
        return false;
    } else if (e1->get_type() != type || e2->get_type() != type) {
        classtable->semant_error(c->get_filename(), e1) << "non-" << type << " arguments: " << e1->get_type() << " " << op << " " << e2->get_type() << std::endl;
        return false;
    } else {
        return true;
    }
}

bool plus_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Int;
    return binary_op_typecheck(c, classtable, attr_tbl, e1, e2, type, idtable.add_string("+"));
}

bool sub_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Int;
    return binary_op_typecheck(c, classtable, attr_tbl, e1, e2, type, idtable.add_string("-"));
}

bool mul_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Int;
    return binary_op_typecheck(c, classtable, attr_tbl, e1, e2, type, idtable.add_string("*"));
}

bool divide_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Int;
    return binary_op_typecheck(c, classtable, attr_tbl, e1, e2, type, idtable.add_string("/"));
}

bool lt_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Bool;
    return binary_op_typecheck(c, classtable, attr_tbl, e1, e2, Int, idtable.add_string("<"));
}

bool leq_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Bool;
    return binary_op_typecheck(c, classtable, attr_tbl, e1, e2, Int, idtable.add_string("<="));
}

bool eq_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Bool;
    bool e1_success = e1->typecheck(c, classtable, attr_tbl);
    bool e1_is_constant = e1->get_type() == Int || e1->get_type() == Str || e1->get_type() == Bool;

    bool e2_success = e2->typecheck(c, classtable, attr_tbl);
    bool e2_is_constant = e2->get_type() == Int || e2->get_type() == Str || e2->get_type() == Bool;

    bool constant_mismatch = (e1_is_constant || e2_is_constant) && e1->get_type() != e2->get_type();

    if (!e1_success || !e2_success) {
        return false;
    } else if (constant_mismatch) {
        classtable->semant_error(c->get_filename(), e1) << "Illegal comparison with a basic type" << std::endl;
        return false;
    } else {
        return true;
    }
}

// unary operators

bool unary_op_typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl, Expression e1, Symbol type, Symbol op) {
    bool e1_success = e1->typecheck(c, classtable, attr_tbl);
    if (!e1_success) {
        return false;
    } else if (e1->get_type() != type) {
        classtable->semant_error(c->get_filename(), e1) << "Argument of '" << op << "' has type " << e1->get_type() << " instead of " << type << std::endl;
        return false;
    } else {
        return true;
    }
}

bool comp_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Bool;
    return unary_op_typecheck(c, classtable, attr_tbl, e1, type, idtable.add_string("not"));
}

bool neg_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Int;
    return unary_op_typecheck(c, classtable, attr_tbl, e1, type, idtable.add_string("~"));
}

bool new__class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = type_name == SELF_TYPE ? c->get_name() : type_name;
    if (classtable->class_for_symbol(type) == NULL) {
        classtable->semant_error(c->get_filename(), this) << "'new' used with undefined class " << type << std::endl;
        return false;
    }
    return true;
}

bool isvoid_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Bool;
    return e1->typecheck(c, classtable, attr_tbl);
}

bool assign_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    Symbol attr_type = attr_tbl->lookup(name);
    bool expr_success = expr->typecheck(c, classtable, attr_tbl);

    if (attr_type == NULL) {
        classtable->semant_error(c->get_filename(), this) << name << " is undefined" << std::endl;
        return false;
    } else if (!expr_success) {
        return false;
    } else if (!classtable->conforms(expr->get_type(), attr_type)) {
        classtable->semant_error(c->get_filename(), expr) << "Assignment expression type " << expr->get_type() << " does not conform to type " << attr_type << std::endl;
        return false;
    } else {
        type = expr->get_type();
        return true;
    }
}

bool cond_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    bool pred_success = pred->typecheck(c, classtable, attr_tbl);
    bool then_success = then_exp->typecheck(c, classtable, attr_tbl);
    bool else_success = else_exp->typecheck(c, classtable, attr_tbl);
    if (!pred_success || !then_success || !else_success) { return false; }
    if (pred->get_type() != Bool) {
        classtable->semant_error(c->get_filename(), pred) << "If predicate must be of type Bool" << std::endl;
        return false;
    }
    type = classtable->least_type(then_exp->get_type(), else_exp->get_type());
    return true;
}

bool loop_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    type = Object;
    bool pred_success = pred->typecheck(c, classtable, attr_tbl);
    bool body_success = body->typecheck(c, classtable, attr_tbl);
    if (!pred_success || !body_success) { return false; }
    if (pred->get_type() != Bool) {
        classtable->semant_error(c->get_filename(), pred) << "Loop predicate must be of type Bool" << std::endl;
        return false;
    }
    return true;
}

bool let_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    if (init->has_expression()) {
        bool init_success = init->typecheck(c, classtable, attr_tbl);
        if (!init_success) { return false; }
        if (!classtable->conforms(init->get_type(), type_decl)) {
            classtable->semant_error(c->get_filename(), init) << "Initialization type " << init->get_type() << " for let " << identifier << " does not conform to type " << type_decl << std::endl;
            return false;
        }
    }
    attr_tbl->enterscope();
    attr_tbl->addid(identifier, type_decl);
    bool body_success = body->typecheck(c, classtable, attr_tbl);
    if (body_success) {
        type = body->get_type();
    }
    attr_tbl->exitscope();
    return body_success;
}

bool block_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    Symbol last_type;
    for(int i = body->first(); body->more(i); i = body->next(i)) {
        bool success = body->nth(i)->typecheck(c, classtable, attr_tbl);
        if (!success) { return false; }
        last_type = body->nth(i)->get_type();
    }
    type = last_type;
    return true;
}

bool no_expr_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    return false;
}

bool object_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    Symbol attr_type = attr_tbl->lookup(name);
    if (attr_type == NULL) {
        classtable->semant_error(c->get_filename(), this) << name << " is undefined" << std::endl;
        return false;
    }
    type = attr_type;
    return true;
}

bool dispatch_class::typecheck(Class_ c, ClassTable* classtable, SymbolTable<Symbol, Entry>* attr_tbl) {
    bool expr_success = expr->typecheck(c, classtable, attr_tbl);
    if (!expr_success) { return false; }
    std::vector<Symbol> param_types;
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        if (!actual->nth(i)->typecheck(c, classtable, attr_tbl)) {
            return false;
        }
        param_types.push_back(actual->nth(i)->get_type());
    }


    Symbol dispatch_to_sym = expr->get_type() == SELF_TYPE ? c->get_name() : expr->get_type();
    Class_ dispatch_class = classtable->class_for_symbol(dispatch_to_sym);
    // ask the class if there is a method that matches the type
    // TODO: handle SELF_TYPE
    if (!dispatch_class->has_method_of_type(name, expr->get_type(), param_types, classtable)) {
        classtable->semant_error(c->get_filename(), this) << "There is no method named " << name << " on type " << expr->get_type() << " which take the specified parameters" << std::endl;
        return false;
    }
    type = expr->get_type();
    return true;
}


// TODO: static_dispatch_class
// TODO: typcase_class