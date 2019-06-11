struct W
{
  int x;
}

int fn(int x)
{
  W[1] a = W(0);
  int r = 
    (a[0].x = x);
  return r;
}

pragma(msg, fn(16));
