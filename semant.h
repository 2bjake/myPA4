#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <set>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::set<Symbol> class_symbols;
  SymbolTable<Symbol, Class__class> *tbl;

  void add_class(Class_ c);

public:
  ClassTable(Classes);

  Class_ class_for_symbol(Symbol symbol) { return tbl->lookup(symbol); }
  bool conforms(Symbol c, Symbol super);
  Symbol least_type(Symbol a, Symbol b);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

