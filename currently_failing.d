struct S
{
    size_t[4] a = 1;
    size_t[4] b = 4;
}

S makeS()
{
    S s;
    size_t i;


    bool buildNext()
    {
        if (i < S.a.length + S.b.length)
        {
            if (i < S.a.length)
            {
                s.a[i] = i;
            }
            else
            {
                s.b[i - S.a.length] = i;
            }

            i++;
            return true;
        }
        return false;
    }

    while(buildNext()) {}

    return s;
}

pragma(msg, makeS());


struct S2
{
  int x;
  int[2] a;
}

S2 fn2()
{
    S2 s = S2.init;

    void initS()
    {
        s.x = 1;
        s.a[0] = 2;
        s.a[1] = 3;
    }

    initS();
    return s;
}


// pragma(msg, fn2());

//static assert(fn2() == S2(1, [2, 3]));
