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
    int g = 12;

    int wrap(int x)
    {
        try
        {
            throw new VoidExp();
        }
        catch(IntExp e)
        {
            g = -44;
        }
        return 99;
    }

    try
    {
//         fthrow(x);
              wrap(x);
//        throw new IntExp(2);
    }
    catch (IntExp e)
    {
        return e.value + g;
    }
    catch (ValueExp e)
    {
        g = 8;
    }
    return g;
    // assert(0);
}

pragma(msg, fn(22));

