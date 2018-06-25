
/// iterative binary search

import p4m;


pragma(msg, "primes.length: ", primes.length);

int ibs_find (const (uint[]) arr, uint v)
{
    return binarySearch(arr, 0, cast(int)(arr.length), v);
}

static immutable uint[] array = [1,3,5,7,11,13,17,21];

int binarySearch(const (uint[]) arr, int l, int r, uint x)
{
    while (l <= r)
    {
        int m = l + (r-l)/2;
 
        // Check if x is present at mid
        if (arr[m] == x)
            return m;
 
        // If x greater, ignore left half
        if (arr[m] < x)
            l = m + 1;
 
        // If x is smaller, ignore right half
        else
            r = m - 1;
    }
 
    // if we reach here, then element was
    // not present
    return -1;
}
 
//static assert(ibs_find(array, 21) == 7);
pragma(msg, ibs_find(primes, 3904591));
pragma(msg, ibs_find(primes, 1811041));

