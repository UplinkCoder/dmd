int fn(int x)
{
  int[1] a = 0;
  int r = 
    (a[0] = x);
  return r;
}

pragma(msg, fn(16));
