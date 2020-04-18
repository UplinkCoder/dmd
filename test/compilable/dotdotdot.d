template Seq(typeArgs...) { alias Seq = typeArgs; }
alias Tup = Seq!(0, 1, 2);
alias Tup2 = Seq!(4, 5, 6);
alias Tup3 = Seq!(7, 8, 9);
static immutable int[3] x = [ 10, 20, 30 ];
static assert([ (Tup + 3)... ] == [3, 4, 5]);
static assert([ (x[Tup])... ] == [10, 20, 30]);


pragma(msg, x[Tup]...);

static assert([(Tup + (Tup2 + Tup3)...)...]  == [0 + 4 + 7, 1 + 5 + 8, 2 + 6 + 9]);
