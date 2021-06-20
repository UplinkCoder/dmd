module donotcare;

void __itt_sync_create (void *addr, const char *objtype, const char *objname, int attribute);
import core.reflect.decl;

//
/+    

    /**
    * @brief Quit spin loop without acquiring spin object
    */
    void function (void* addr) __itt_sync_cancel;
    /**
    * @brief Successful spin loop completion (sync object acquired)
    */
    void function (void* addr) __itt_sync_acquired;
    /**
    * @brief Start sync object releasing code. Is called before the lock release call.
    */
    void function (void* addr) __itt_sync_releasing;
    
    /**
    * @brief Start sync object spinloc k code.
    */
    void function (void* addr) __itt_sync_prepare;
+/

static immutable dummy_decl = declarationsFromTokenString(q{
extern (C) @nogc pure nothrow __gshared {
    /**@brief the dummy function*/
    int dummy (void*) {};
}});

static immutable var_decl = declarationsFromTokenString(q{
    /**@brief the dummy variable*/
    string var;
});

static assert (
  dummy_decl[0].name == "dummy" &&
  // for some reasons the comment gets a linebreak therefore we have to slice it off
  dummy_decl[0].comment[0 .. $-1] == "@brief the dummy function"
  && (cast(FunctionDeclaration)dummy_decl[0]).type.returnType.identifier == "int"
  && dummy_decl[0].kind() == DeclarationKind.FunctionDeclaration
);

pragma(msg, dummy_decl);
pragma(msg, var_decl);

static assert(
 declarationsFromTokenString(q{
  extern (C) @nogc pure nothrow __gshared {
      void dummy (void*) {};
      /**this is my Cool ddoc comment*/
      string myCoolFunctionTheSecond() {};
  }
 })
[1].comment[0 .. $-1] == "this is my Cool ddoc comment");

import core.reflect.node;

double myVar;
float myVar2;

static immutable sym_node = nodeFromName("myVar");
import helpers;
static immutable sym_decl = getDeclarationFromName("myVar");
static immutable sym_decls = getDeclarationsFromNames(["myVar", "myVar2", "__itt_sync_create"]);


/// comment
int[2] myArr;

import core.reflect.type;
import core.reflect.node;
static immutable arr_node = nodeFromName("myArr");

pragma(msg, "x before decl: ", nodeFromName("x"));
typeof(myArr)* x;
pragma(msg, "x after decl:", nodeFromName("x"));

@(core.reflect) immutable(Type) getType(immutable Node node)
{
  if (immutable vd = cast(immutable VariableDeclaration)node)
  {
     return vd.type;
  }

  return null;
}

static immutable arr_type = cast(immutable TypeArray) getType(arr_node);
static assert(
    arr_type.identifier == "int[2]"
    && arr_type.nextOf.identifier == "int"
    && arr_type.dim == 2
);

import core.reflect.dscope;
import core.reflect.node;

void main()
{
  import std.stdio;
  writeln((cast(Declaration)arr_node).name);
}
