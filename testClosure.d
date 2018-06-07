struct R 
{
    int delegate(int) dg;
    int rv;
}


int square_of_x_plus_x(int x) pure
{
    static int echo(int x) {return x;}

    int fnC(int y) pure
    {
        auto z = (x * y);
        x = 12;
        return z;
    }
    return x + fnC(x);
}

pragma(msg, square_of_x_plus_x(7));
