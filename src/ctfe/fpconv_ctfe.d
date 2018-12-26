
/** Port of grisu2 implementation by night-shift
   https://github.com/night-shift/fpconv
   Converted to (CTFEable) D by Stefan Koch */

module fpconv_ctfe;

enum npowers = 87;
enum steppowers = 8;
enum firstpower = -348; /* 10 ^ -348 */

enum expmax = -32;
enum expmin = -60;

struct Fp {
    ulong frac;
    int exp;
};

static immutable Fp[npowers] powers_ten = [
    { 18054884314459144840U, -1220 }, { 13451937075301367670U, -1193 },
    { 10022474136428063862U, -1166 }, { 14934650266808366570U, -1140 },
    { 11127181549972568877U, -1113 }, { 16580792590934885855U, -1087 },
    { 12353653155963782858U, -1060 }, { 18408377700990114895U, -1034 },
    { 13715310171984221708U, -1007 }, { 10218702384817765436U, -980 },
    { 15227053142812498563U, -954 }, { 11345038669416679861U, -927 },
    { 16905424996341287883U, -901 }, { 12595523146049147757U, -874 },
    { 9384396036005875287U, -847 }, { 13983839803942852151U, -821 },
    { 10418772551374772303U, -794 }, { 15525180923007089351U, -768 },
    { 11567161174868858868U, -741 }, { 17236413322193710309U, -715 },
    { 12842128665889583758U, -688 }, { 9568131466127621947U, -661 },
    { 14257626930069360058U, -635 }, { 10622759856335341974U, -608 },
    { 15829145694278690180U, -582 }, { 11793632577567316726U, -555 },
    { 17573882009934360870U, -529 }, { 13093562431584567480U, -502 },
    { 9755464219737475723U, -475 }, { 14536774485912137811U, -449 },
    { 10830740992659433045U, -422 }, { 16139061738043178685U, -396 },
    { 12024538023802026127U, -369 }, { 17917957937422433684U, -343 },
    { 13349918974505688015U, -316 }, { 9946464728195732843U, -289 },
    { 14821387422376473014U, -263 }, { 11042794154864902060U, -236 },
    { 16455045573212060422U, -210 }, { 12259964326927110867U, -183 },
    { 18268770466636286478U, -157 }, { 13611294676837538539U, -130 },
    { 10141204801825835212U, -103 }, { 15111572745182864684U, -77 },
    { 11258999068426240000U, -50 }, { 16777216000000000000U, -24 },
    { 12500000000000000000U, 3 }, { 9313225746154785156U, 30 },
    { 13877787807814456755U, 56 }, { 10339757656912845936U, 83 },
    { 15407439555097886824U, 109 }, { 11479437019748901445U, 136 },
    { 17105694144590052135U, 162 }, { 12744735289059618216U, 189 },
    { 9495567745759798747U, 216 }, { 14149498560666738074U, 242 },
    { 10542197943230523224U, 269 }, { 15709099088952724970U, 295 },
    { 11704190886730495818U, 322 }, { 17440603504673385349U, 348 },
    { 12994262207056124023U, 375 }, { 9681479787123295682U, 402 },
    { 14426529090290212157U, 428 }, { 10748601772107342003U, 455 },
    { 16016664761464807395U, 481 }, { 11933345169920330789U, 508 },
    { 17782069995880619868U, 534 }, { 13248674568444952270U, 561 },
    { 9871031767461413346U, 588 }, { 14708983551653345445U, 614 },
    { 10959046745042015199U, 641 }, { 16330252207878254650U, 667 },
    { 12166986024289022870U, 694 }, { 18130221999122236476U, 720 },
    { 13508068024458167312U, 747 }, { 10064294952495520794U, 774 },
    { 14996968138956309548U, 800 }, { 11173611982879273257U, 827 },
    { 16649979327439178909U, 853 }, { 12405201291620119593U, 880 },
    { 9242595204427927429U, 907 }, { 13772540099066387757U, 933 },
    { 10261342003245940623U, 960 }, { 15290591125556738113U, 986 },
    { 11392378155556871081U, 1013 }, { 16975966327722178521U, 1039 },
    { 12648080533535911531U, 1066 }
];

Fp find_cachedpow10(int exp, int* k)
{
    const double one_log_ten = 0.30102999566398114;

    int approx = cast(int) (-(exp + npowers) * one_log_ten);
    int idx = (approx - firstpower) / 8;

    while(1) {
        int current = exp + powers_ten[idx].exp + 64;

        if(current < expmin) {
            idx++;
            continue;
        }

        if(current > expmax) {
            idx--;
            continue;
        }

        *k = (firstpower + idx * steppowers);

        return powers_ten[idx];
    }
}

static immutable ulong[20] tens = [
    10000000000000000000U, 1000000000000000000U, 100000000000000000U,
    10000000000000000U, 1000000000000000U, 100000000000000U,
    10000000000000U, 1000000000000U, 100000000000U,
    10000000000U, 1000000000U, 100000000U,
    10000000U, 1000000U, 100000U,
    10000U, 1000U, 100U,
    10U, 1U
];

ulong get_dbits(double d)
{
    ulong bits = *cast(ulong*)&d;
    return bits;
}

Fp build_fp(double d)
{
    ulong bits = get_dbits(d);

    Fp fp;
    fp.frac = bits & 0x000FFFFFFFFFFFFFU;
    fp.exp = (bits & 0x7FF0000000000000U) >> 52;

    if(fp.exp) {
        fp.frac += 0x0010000000000000U;
        fp.exp -= (1023 + 52);

    } else {
        fp.exp = -(1023 + 52) + 1;
    }

    return fp;
}

void normalize(Fp* fp)
{
    while ((fp.frac & 0x0010000000000000U) == 0) {
        fp.frac <<= 1;
        fp.exp--;
    }

    int shift = 64 - 52 - 1;
    fp.frac <<= shift;
    fp.exp -= shift;
}

void get_normalized_boundaries(Fp* fp, Fp* lower, Fp* upper)
{
    upper.frac = (fp.frac << 1) + 1;
    upper.exp = fp.exp - 1;

    while ((upper.frac & (0x0010000000000000U << 1)) == 0) {
        upper.frac <<= 1;
        upper.exp--;
    }

    int u_shift = 64 - 52 - 2;

    upper.frac <<= u_shift;
    upper.exp = upper.exp - u_shift;


    int l_shift = fp.frac == 0x0010000000000000U ? 2 : 1;

    lower.frac = (fp.frac << l_shift) - 1;
    lower.exp = fp.exp - l_shift;


    lower.frac <<= lower.exp - upper.exp;
    lower.exp = upper.exp;
}

Fp multiply(Fp* a, Fp* b)
{
    const ulong lomask = 0x00000000FFFFFFFF;

    ulong ah_bl = (a.frac >> 32) * (b.frac & lomask);
    ulong al_bh = (a.frac & lomask) * (b.frac >> 32);
    ulong al_bl = (a.frac & lomask) * (b.frac & lomask);
    ulong ah_bh = (a.frac >> 32) * (b.frac >> 32);

    ulong tmp = (ah_bl & lomask) + (al_bh & lomask) + (al_bl >> 32);

    tmp += 1U << 31;

    Fp fp = {
        ah_bh + (ah_bl >> 32) + (al_bh >> 32) + (tmp >> 32),
        a.exp + b.exp + 64
    };

    return fp;
}

void round_digit(char* digits, int ndigits, ulong delta, ulong rem, ulong kappa, ulong frac)
{
    while (rem < frac && delta - rem >= kappa &&
           (rem + kappa < frac || frac - rem > rem + kappa - frac)) {

        digits[ndigits - 1]--;
        rem += kappa;
    }
}

int generate_digits(const Fp* fp, const Fp* upper, const Fp* lower, char* digits, int* K)
{
    ulong wfrac = upper.frac - fp.frac;
    ulong delta = upper.frac - lower.frac;

    Fp one;
    one.frac = 1UL << -upper.exp;
    one.exp = upper.exp;

    ulong part1 = upper.frac >> -one.exp;
    ulong part2 = upper.frac & (one.frac - 1);

    int idx = 0, kappa = 10;

    for(auto div_idx = 10; kappa > 0; div_idx++) {

        ulong div = tens[div_idx];
        const digit = part1 / div;

        if (digit || idx) {
            digits[idx++] = cast(char) (digit + '0');
        }

        part1 -= digit * div;
        kappa--;

        ulong tmp = (part1 <<-one.exp) + part2;
        if (tmp <= delta) {
            *K += kappa;
            round_digit(digits, idx, delta, tmp, div << -one.exp, wfrac);

            return idx;
        }
    }


    auto unit_idx = 18;

    while(1) {
        part2 *= 10;
        delta *= 10;
        kappa--;

        const digit = part2 >> -one.exp;
        if (digit || idx) {
            digits[idx++] = cast(char) (digit + '0');
        }

        part2 &= one.frac - 1;
        if (part2 < delta) {
            *K += kappa;
            round_digit(digits, idx, delta, part2, one.frac, wfrac * tens[unit_idx]);

            return idx;
        }

        unit_idx--;
    }
}

static int grisu2(double d, char* digits, int* K)
{
    Fp w = build_fp(d);

    Fp lower, upper;
    get_normalized_boundaries(&w, &lower, &upper);

    normalize(&w);

    int k;
    Fp cp = find_cachedpow10(upper.exp, &k);

    w = multiply(&w, &cp);
    upper = multiply(&upper, &cp);
    lower = multiply(&lower, &cp);

    lower.frac++;
    upper.frac--;

    *K = -k;

    return generate_digits(&w, &upper, &lower, digits, K);
}

static int emit_digits(char* digits, int ndigits, char* dest, int K, bool neg)
{
    int exp = ((K + ndigits - 1) < 0 ? -(K + ndigits - 1) : (K + ndigits - 1));


    if(K >= 0 && (exp < (ndigits + 7))) {
        dest[0 .. ndigits] = digits[0 .. ndigits]; 
        //memcpy(dest, digits, ndigits);
        dest[ndigits .. ndigits + K] =  0;
        //memset(dest + ndigits, '0', K);

        return ndigits + K;
    }


    if(K < 0 && (K > -7 || exp < 4)) {
        int offset = ndigits - ((K) < 0 ? -(K) : (K));

        if(offset <= 0) {
            offset = -offset;
            dest[0] = '0';
            dest[1] = '.';

            //memset(dest + 2, '0', offset);
            dest[2 .. 2 + offset] = 0;
            //memcpy(dest + offset + 2, digits, ndigits);
            dest[2 + offset .. 2 + offset + ndigits] = digits[0 .. ndigits];

            return ndigits + 2 + offset;


        } else {
            //memcpy(dest, digits, offset);
            dest[0 .. offset] = digits[0 .. offset];
            dest[offset] = '.';
            //memcpy(dest + offset + 1, digits + offset, ndigits - offset);
            dest[offset + 1 .. 1 + ndigits] = (digits + offset)[0 .. ndigits - offset];

            return ndigits + 1;
        }
    }


    ndigits = ((ndigits) < (18 - neg) ? (ndigits) : (18 - neg));

    int idx = 0;
    dest[idx++] = digits[0];

    if(ndigits > 1) {
        dest[idx++] = '.';
        //memcpy(dest + idx, digits + 1, ndigits - 1);
        dest[idx .. idx + ndigits - 1] = digits[1 .. ndigits];
        idx += ndigits - 1;
    }

    dest[idx++] = 'e';

    char sign = K + ndigits - 1 < 0 ? '-' : '+';
    dest[idx++] = sign;

    auto _cent = 0;

    if(exp > 99) {
        _cent = exp / 100;
        dest[idx++] = cast(char) (_cent + '0');
        exp -= _cent * 100;
    }
    if(exp > 9) {
        int dec = exp / 10;
        dest[idx++] = cast(char) (dec + '0');
        exp -= dec * 10;

    } else if(_cent) {
        dest[idx++] = '0';
    }

    dest[idx++] = exp % 10 + '0';

    return idx;
}
/+
static int filter_special(double fp, char* dest)
{
    if(fp == 0.0) {
        dest[0] = '0';
        return 1;
    }

    ulong bits = get_dbits(fp);

   bool nan = (bits & 0x7FF0000000000000U) == 0x7FF0000000000000U;

    if(!nan) {
        return 0;
    }

    if(bits & 0x000FFFFFFFFFFFFFU) {
        dest[0] = 'n'; dest[1] = 'a'; dest[2] = 'n';

    } else {
        dest[0] = 'i'; dest[1] = 'n'; dest[2] = 'f';
    }

    return 3;
}
+/
int fpconv_dtoa(double d, ref char[24] dest)
{
    char[18] digits;
    int str_len = 0;
    bool neg = 0;
    ulong bits = (*(cast(ulong*)&d));
    bool is_nan = ((bits & 0x7FF0000000000000U) == 0x7FF0000000000000U);
    
    if(bits & (1UL << 63)) {
        dest[0] = '-';
        str_len = 1;
        neg = 1;
    }

    if (d == 0.0)
    {
       dest[0] = '0';
       return 1;
    }


    if (is_nan) 
    {
        // this case is unlikely
        // we are going to define it as delegate
        // to give the compiler a chance to move it out
        () {
            str_len = 3;
            if (bits & 0x000FFFFFFFFFFFFFU)
            {
                dest[0 .. 3] = "nan";
            }
            else
            {  
                if (neg)
                {
                    dest[1 .. 4] = "inf";
                    str_len = 4;
                }
                else
                {
                    dest[0 .. 3] = "inf";
                }                
            }
        } ();
        
        return str_len;
    }

    int K = 0;
    auto ndigits = grisu2(d, &digits[0], &K);
  
    str_len += emit_digits(&digits[0], ndigits, &dest[0] + str_len, K, neg);
    return str_len;
}


string fpconv_dtoa(double d)
{
    char[24] buffer;
    const len = fpconv_dtoa(d, buffer);
    auto result = new char[](len);
    result[0 .. len] = buffer[0 .. len];
    return cast(string) result;
}

static assert (() {
    char[24] buffer;
    int len = fpconv_dtoa(3.14159, buffer);
    return buffer[0 .. len].idup;
} () == "3.14159");

static assert (fpconv_dtoa(0.3) == "0.3");
static assert (fpconv_dtoa(-double.infinity) == "-inf");
static assert (fpconv_dtoa(double.infinity) == "inf");
static assert (fpconv_dtoa(double.nan) == "nan");
static assert (fpconv_dtoa(1.3f) == "1.3");
static assert (fpconv_dtoa(65.221) == "65.221");
static assert (fpconv_dtoa(1.3) == "1.3");
static assert (fpconv_dtoa(0.3) == "0.3");


