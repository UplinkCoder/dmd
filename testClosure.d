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
            x += 10;
            return z;
        }
        return fnC();
    }
    return passThrough(x) + x;
}

static assert(square_of_x_plus_x(7) == (7*7)+17);
static assert(square_of_x_plus_x(5) == (5*5)+15);
