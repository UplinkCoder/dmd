template Seq(typeArgs...) { alias Seq = typeArgs; }
alias Tup = Seq!(0, 1, 2);
alias Tup2 = Seq!(4, 5, 6);
alias Tup3 = Seq!(7, 8, 9);
static immutable int[3] x = [ 10, 20, 30 ];
static assert([ (Tup + 3)... ] == [3, 4, 5]);
static assert([ (x[Tup])... ] == [10, 20, 30]);
static assert([(Tup + (Tup2 + Tup3)...)...]  == [0 + 4 + 7, 1 + 5 + 8, 2 + 6 + 9]);

align(1)
struct S
{
   int a;
   char c;
   int h;
}

struct S1
{
   int b;
}

alias structs = Seq!(S, S1);

static assert([structs.sizeof...] == [12, 4]);
static assert([__traits(identifier, structs)...] == ["S", "S1"]);
// pragma(msg, [structs.tupleof....stringof...]); 

struct MetaInfo
{
    string[] struct_names;
//    string[][] struct_members;
    uint[]     number_of_members;  
    size_t[] struct_size;
}
// pragma(msg, "(structs.tupleof...)", (structs.tupleof...).stringof);

static immutable info = MetaInfo(   [ __traits(identifier, structs)...],
//                                    [ [ (structs.tupleof...).stringof]...],
                                    [ structs.tupleof.length... ],
                                    [structs.sizeof...]  ); 
pragma(msg, info);

static assert(__ArrayEq(info.struct_names, ["S", "S1"]));  // can't do == because life's not nice
static assert(info.number_of_members == [3,1]);
static assert(info.struct_size == [12,4]);

/+ broken by not calling semantic directly on tupleof...
static assert ([(SC.tupleof....sizeof...)] == 
                               [1,      1,              1,      1,      4]);
static assert ([(SC.tupleof....stringof...)] == 
                                  ["x",        "y",         "z",    "w",     "f"]);
extern(C) align(4) struct SC { char x = 4; char y = 12; char z; char w; float f = 84.3; }
+/
