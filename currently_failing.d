/+
struct S
{
    int beginning = 63;
    int[4] a = 1;
    int[4] b = 4;
    int end = 256;
}

pragma(msg, "S.sizeof: ", S.sizeof); 

S makeS()
{
    S s;
    int i;


    bool buildNext()
    {

        if (i < S.a.length + S.b.length)
        {
            assert(s.beginning == 63);
            assert(s.end == 256);

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

+/

struct S2
{
  int x = 64;
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


static assert(fn2() == S2(1, [2, 3]));
