class ValueExp : Exception
{
    this(string msg) { super(msg); }
}

class IntExp : ValueExp
{
  this(int x)
  {
    super("");
    this.value = x;
  }

  int value;
}


void fthrow(int x)
{
    throw new IntExp(x);
}


int fn(int x)
{
    int g = 12;

    int wrap(int x)
    {
        try
        {
            throw new IntExp(x);
        }
        catch(IntExp e)
        {
            g += -44;
//            throw e;
        }
        return 99;
    }

    try
    {
              wrap(x);
    }
    catch (IntExp e)
    {
        return e.value + g;
    }
    return g;
    // assert(0);
}

pragma(msg, fn(22));

// pragma(msg, () { static class C { int y; this(int y) { this.y = y; }} static class S { int i = 73; string x = "x"; C c = new C(64); } S s = new S(); s.x = "Hello"; s.c = new C(32); return s; } ());
