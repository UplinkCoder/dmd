double[] ta()
{
    return [12.0, 2.0];
}

double[] f(float d, int s)
{
    double v = d * s;
    double[] da = [v];
    return da;
}

pragma(msg, f(1, 2));
