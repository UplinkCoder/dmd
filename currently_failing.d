static immutable int[] four = [1, 2, 3, 4];

int fn()
{
    return *(&four[2]);
}

static assert(fn() == 3);


