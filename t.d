long echo ( long l) { return l; }

//static assert(() { return ((1L << 52) | 16).echo.echo; } () == ((1L << 52) | 16));

struct S
{
  int[3] a = 13;
}

static assert (() {
    S s = S();
    return s.a;
} () == [13,13,13]);

pragma(msg, cast(string)[86,
111,
105,
100,
69,
120,
112,
32,
109,
101,
115,
115,
97,
103,
101,
]);
