double[] f(float d, int s)
{
    float a = 2;
    float b = 5;
    double c;

    c = a * b;

    float v = d * s;
    double[] da = [v, b];
    return da;
}

pragma(msg, f(2, 24));
