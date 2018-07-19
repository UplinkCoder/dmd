static immutable int[] four = [1, 2, 3, 4];

<<<<<<< 75656c12a57cdc4f6b43df787295aea66221f7dc
int fn()
{
    return *(&four[2]);
=======
    auto last = &four[0] + (four.length - 1);
    assert(last == &four[$ - 1]);
    
    return fn2(&four[0]) + *(&four[0]);
>>>>>>> Update currently failing
}

static assert(fn() == 3);


