class NoExp : Exception { this() { super("NoExp"); }}

class IntExp : Exception
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
    throw new NoExp();
}

int fn(int x)
{
    try
    {
        fthrow(x);
    }
    catch (IntExp e)
    {
        auto xx = e.value;
        return e.value + 64;
    }
    assert(0);
}

pragma(msg, fn(22));
