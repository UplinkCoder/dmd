class ValueExp : Exception
{
    this(string msg) { super(msg); }
}

class VoidExp : Exception
{
    this() { super(""); }
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
    int g = 0;
    void wrap(int x)
    {
        try
        {
            return fthrow(x);
        } catch (VoidExp e) { g = 231; }
          catch (ValueExp e) { g = 89; assert(0); }
       {
       }
    }


    try
    {
    //    fthrow(x);
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

