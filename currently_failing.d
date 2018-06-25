static immutable four = [4, 4, 4, 4];

int fn(const int* x)
{
      return x[2];
}

static assert(fn(&four[0]) == 4);
