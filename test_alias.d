/+
string F(alias y)
{
    return y.stringof;
}

size_t SO(alias y)
{
    return y.sizeof;
}

//static assert(F!(ulong) == "ulong");
static assert(F!(uint) == "uint");
//static assert(F!(float) == "uint");

//pragma(msg, SO!(real));
//pragma(msg, SO!(int));
//pragma(msg, SO!(ulong));


alias[] typeMap(alias[] types, alias function(alias) mapFn)
{
    alias[] result;
    foreach(i, t;types)
    {
        result[i] = mapFn(t);
    }
    return result;
}
+/
string fqn(alias t)
{
    string result = t.stringof;
    alias p;
    p = t;
    bool good = is(typeof(__traits(parent, p)));
    while(good)
    {
        p = __traits(parent, p);
        result = __traits(identifier, p) ~ "." ~ result;
        good = is(typeof(__traits(parent, p)));

    }

    return cast(string) result;
}


string fqnStringof(alias t)
{
    string result = t.stringof;
    alias p;
    p = t;
    bool good = is(typeof(__traits(parent, p)));
    while(good)
    {
        p = __traits(parent, p);
        result = p.stringof ~ "." ~ result;
        good = is(typeof(__traits(parent, p)));

    }

    return cast(string) result;
}

struct S 
{
    struct X
    {
        int xx;        
        static assert( fqn!xx == __MODULE__ ~ ".S.X.xx" );
        static assert( fqnStringof!xx == "module " ~ __MODULE__ ~ ".S.X.xx" );
    }
}


