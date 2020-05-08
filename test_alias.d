
string F(alias y)
{
    return y.stringof;
}

size_t SO(alias y)
{
    return y.sizeof;
}

//static assert(F!(ulong) == "ulong");
//static assert(F!(uint) == "uint");
//static assert(F!(float) == "uint");

//pragma(msg, SO!(real));
//pragma(msg, SO!(int));
//pragma(msg, SO!(ulong));


string fqn(alias t)
{
    char[] result;
    alias parent;
    parent = t;
    while(is(parent) || is(typeof(parent)))
    {
        result = __traits(identifier, parent) ~ "." ~ result;
        // parent = __traits(parent, parent);
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


