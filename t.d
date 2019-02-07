long echo ( long l) { return l; }

//static assert(() { return ((1L << 52) | 16).echo.echo; } () == ((1L << 52) | 16));

struct S
{
  int i1, i11;
  int[4] a = 12;
}


int lp(S* s)
{
    auto i = s.i1 + s.i11;
    s.i1 = 0; s.i11 = 0;
    s.a[0] = 1; s.a[3] = 3;
    return i;
}

pragma(msg, () { 
    S s = S(64, 128);
    S s2 = S(32, 32);
    return S(lp(&s), lp(&s2) + lp(&s2), s.a);
} ());
// should be 
// S(192, 64, [1, 12, 12, 3]);

