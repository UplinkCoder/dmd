
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
        s.a = 2;
        s.a[0] = 2;
        s.a[1] = 3;
    }

    initS();
    return s;
}


static assert(fn2() == S2(1, [2, 3]));

/+
/**************************************************
    3901 Arbitrary struct assignment, ref return
**************************************************/

struct ArrayRet
{
    int x;
}

int arrayRetTest(int z)
{
    ArrayRet[6] w = ArrayRet(0);
    int q = (w[3].x = z);
    return q;
}
static assert(arrayRetTest(51) == 51);

bool test6178d()
{
    // AA value setting through implicit ctor call + alias this

    int ctor;
    struct S
    {
        this(int n) { ++ctor; value = n; }

        int value;
        alias value this;
    }

    S[int] aa;
    assert(ctor == 0);
    assert(aa.length == 0);

    aa[1] = 0;      // implicit ctor call + blit assign
    assert(aa[1].value == 0 && ctor == 1);
    assert(aa.length == 1);

    aa[1] = 1;      // set through alias this
    assert(aa[1].value == 1 && ctor == 1);
    assert(aa.length == 1);

    return true;
}

static assert(test6178d);
+/
