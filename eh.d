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
    throw new IntExp(x);
}

int fn(int x)
{
    try
    {
        fthrow(x);
    }
    catch (IntExp e)
    {
        return e.value;
    }
    assert(0);
}

pragma(msg, fn(22));
