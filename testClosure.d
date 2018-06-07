struct R 
{
    int delegate(int) dg;
    int rv;
}


int square_of_x_plus_x(int x) pure
{
    int passThrough(int y) pure
    {
        int y2 = x;
        int fnC() pure
        {
            auto z = (x * y);
            x = 12;
            y2 = y * 2;
            return z;
        }
        return fnC();
    }
    return x + passThrough(x);
}

pragma(msg, square_of_x_plus_x(7));
