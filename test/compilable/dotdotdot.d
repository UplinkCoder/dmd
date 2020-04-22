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
   char c = 'x';
   int h;
}

struct S1
{
   int b;
   S s;
}

alias structs = Seq!(S, S1);

static assert([structs.sizeof...] == [12, 16]);
static assert([__traits(identifier, structs)...] == ["S", "S1"]);

struct MetaInfo
{
    string struct_name;
    uint    number_of_members; 
    string[] struct_members;
    uint[] struct_member_offsets;
    size_t struct_size;
}

template GetMetaInfo(T)
{
    enum GetMetaInfo = MetaInfo(
        __traits(identifier, T),
        T.tupleof.length,
        [T.tupleof.stringof...],
        [T.tupleof.offsetof...],
        T.sizeof
    );
}

string structToString(T) (T _struct, uint indent = 1)
{
    import std.conv : to;
    string result;
    const string indent_string =
        (uint indent) { char[] result; result.length = indent * 4; result[] = ' '; return cast(string)result; } (indent);
    static immutable MetaInfo info = GetMetaInfo!(T);
    result = info.struct_name ~ " :: {\n";
    foreach(i, m; _struct.tupleof)
    {
        result ~= indent_string;
        static if (is(typeof(m) == struct))
        {
            result ~= info.struct_members[i] ~ " : " ~ structToString(m, indent + 1);
        }
        else
        {
            result ~= info.struct_members[i] ~ " : "  ~ to!string(m) ~ "\n";
        }
    }
    result ~= indent_string[4 .. $] ~ "}\n";
    return result;
}

pragma(msg, GetMetaInfo!S.structToString);
pragma(msg, GetMetaInfo!S1.structToString);
static assert (GetMetaInfo!(S1).structToString ==
q{MetaInfo :: {
    struct_name : S1
    number_of_members : 2
    struct_members : ["b", "s"]
    struct_member_offsets : [0, 4]
    struct_size : 16
}
});
pragma(msg, structToString(S1.init));


static assert ([(SC.tupleof....sizeof...)] == 
                               [1,      1,              1,      1,      4]);
static assert ([(SC.tupleof....stringof...)] == 
                                  ["x",        "y",         "z",    "w",     "f"]);
extern(C) align(4) struct SC { char x = 4; char y = 12; char z; char w; float f = 84.3; }

