void setVal(long* l)
{
   *l = 64 | 64L << 32;
}

long fn()
{
    long l;
    setVal(&l);
    return l;
}

pragma(msg, fn());
