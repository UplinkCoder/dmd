
int[] f(int n)
{
    return new int[] (n);
}

static assert(f(24).length == 24);

int[] f2()
{
    auto r = new int[] (8); 
    foreach(i; 0 .. 8)
    {
        r[i] = i + 1;
    }
    return r;
}

static assert(f2() == [1,2,3,4,5,6,7,8]);


char[] f3()
{
    auto r = new char[] (4); 
    foreach(char c; 'A' .. 'E')
    {
        r[c - 'A'] = c;
    }
    return r;
}

static assert(f3() == ['A', 'B', 'C', 'D']);


char[] f4()
{
    char[4] l;
    l = 'V';
    char[] r = new char[] (4);
    r[] = l[];
    return r;
}

static assert(f4() == "VVVV");
