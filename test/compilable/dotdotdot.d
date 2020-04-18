template Seq(typeArgs...) { alias Seq = typeArgs; }
alias Tup = Seq!(0, 1, 2);
alias Tup2 = Seq!(4, 5, 6);
alias Tup3 = Seq!(7, 8, 9);
static immutable int[3] x = [ 10, 20, 30 ];
static assert([ (Tup + 3)... ] == [3, 4, 5]);
static assert([ (x[Tup])... ] == [10, 20, 30]);
static assert([(Tup + (Tup2 + Tup3)...)...]  == [0 + 4 + 7, 1 + 5 + 8, 2 + 6 + 9]);

struct S
{
   int a;
   char c;
}

struct S1
{
   int b;
}

alias structs = Seq!(S, S1);

static assert([structs.sizeof...] == [8, 4]);


static assert ([(SC.tupleof.stringof...)] == 
                                  ["x",        "y",         "z",    "w",     "f"]);
static assert ([(SC.tupleof.sizeof...)] == 
                               [1,      1,              1,      1,      4]);
extern(C) align(4) struct SC { char x = 4; char y = 12; char z; char w; float f = 84.3; }
