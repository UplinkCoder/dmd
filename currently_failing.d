static immutable four = [1, 2, 3, 4];
int fn(int idx = 2)
{
    int fn2(const int* x)
    {
      return x[idx];
    }

    return fn2(&four[0]) + *(&four[0]);
}

static assert(fn() == 4);
