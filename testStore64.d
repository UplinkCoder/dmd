void setVal(long* l)
{
   *l |= 64 | 64L << 34;
}

long fn()
{
    long l = 32 | 32L << 32;
    setVal(&l);
    return l;
}

static assert (fn() == (32 | 64 | 32L << 32 | 64L << 34));
