struct R 
{
    int delegate(int) dg;
    int rv;
}


struct CLT0
{
    void *previousClosure;
    int v1;
}


R square(int x)
{
    static int echo(int x) {return x;}

    int fnC(int y)
    {
        auto z = (x * y);
	x = 1;
	return z;
    }
    auto addr = &fnC;
    return R(addr, fnC(echo(x)) + x);
}

pragma(msg, square(5));
