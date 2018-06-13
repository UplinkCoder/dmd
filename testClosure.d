struct R 
{
    int delegate(int) dg;
    int rv;
}


int square_of_x_plus_x(int x) pure
{
    int passThrough(int y) pure
    {
        assert(x == y);
        int y2 = x;
        assert(y2 == y);

        int fnC() pure
        {
            auto z = (x * y);
            assert(y2 == x);
            assert(x == y);
            return z;
        }
        return fnC();
    }
    return x + passThrough(x);
}

pragma(msg, square_of_x_plus_x(7));
static assert(square_of_x_plus_x(5) == (5*5)+5);
