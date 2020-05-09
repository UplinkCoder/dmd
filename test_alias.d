
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


string fqn(alias t)
{
    string result = t.stringof;
    alias p;
    p = t;
    bool good = is(typeof(__traits(parent, p)));
//    bool good = false;
    while(good)
    {
        p = __traits(parent, p);
        result = p.stringof ~ "." ~ result;
//        result = __traits(identifier, parent) ~ "." ~ result;
        good = is(typeof(__traits(parent, p)));

    }

    return cast(string) result;
}

struct S 
{
    struct X
    {
        int xx;
        
        // pragma(msg, fullyQualifiedName!xx);
        pragma(msg, fqn!xx);
        // should be __MODULE__ ~ ".S.X.xx";

    }
}


