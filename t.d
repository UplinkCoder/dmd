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

