/+
bool anySatisfy(bool function (alias) pred, alias[] types ...)
{
    foreach(type; types)
    {
        if (pred!(type))
            return true;
    }
    return false;
}
+/
bool anyConvertsToInt(alias[] types ...)
{
    // return anySatisfy!((alias t) => is(t : int), types);
    foreach(type;types)
    {
        if (is(type : int))
        {
            return true;
        }
    }
    return false;
}

pragma(msg, anyConvertsToInt!(S, S2));


struct S{}
struct S2 {
    int x;
    alias x this;
}
