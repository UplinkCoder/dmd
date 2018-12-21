/**
 * Compiler implementation of the
 * $(LINK2 http://www.dlang.org, D programming language).
 *
 * Copyright:   Copyright (c) 1999-2017 by Digital Mars, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(DMDSRC _impcnvtab.d)
 */

module ddmd.impcnvtab;

import ddmd.mtype;

immutable ENUMTY[TMAX][TMAX] impcnvResult = impCnvTab.impcnvResultTab;
immutable ENUMTY[TMAX][TMAX] impcnvType1 = impCnvTab.impcnvType1Tab;
immutable ENUMTY[TMAX][TMAX] impcnvType2 = impCnvTab.impcnvType2Tab;

private:

struct ImpCnvTab
{
    ENUMTY[TMAX][TMAX] impcnvResultTab;
    ENUMTY[TMAX][TMAX] impcnvType1Tab;
    ENUMTY[TMAX][TMAX] impcnvType2Tab;
}

enum ImpCnvTab impCnvTab = generateImpCnvTab();

ImpCnvTab generateImpCnvTab()
{
    ImpCnvTab impCnvTab;

    // Set conversion tables
    foreach (i; 0 .. cast(size_t)TMAX)
    {
        foreach (j; 0 .. cast(size_t)TMAX)
        {
            impCnvTab.impcnvResultTab[i][j] = Terror;
            impCnvTab.impcnvType1Tab[i][j] = Terror;
            impCnvTab.impcnvType2Tab[i][j] = Terror;
        }
    }

    static void X(ImpCnvTab* impCnvTabPtr,
        ENUMTY t1, ENUMTY t2, ENUMTY nt1, ENUMTY nt2, ENUMTY rt)
    {
        impCnvTabPtr.impcnvResultTab[t1][t2] = rt;
        impCnvTabPtr.impcnvType1Tab[t1][t2] = nt1;
        impCnvTabPtr.impcnvType2Tab[t1][t2] = nt2;
    }

    /* ======================= */

    X(&impCnvTab, Tbool,Tbool,   Tbool,Tbool,    Tbool);
    X(&impCnvTab, Tbool,Tint8,   Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tbool,Tuns8,   Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tbool,Tint16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tbool,Tuns16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tbool,Tint32,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tbool,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tbool,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tbool,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tbool,Tint128, Tint128,Tint128, Tint128);
    X(&impCnvTab, Tbool,Tuns128, Tuns128,Tuns128, Tuns128);

    X(&impCnvTab, Tbool,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tbool,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tbool,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tbool,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tbool,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tbool,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tbool,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tbool,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tbool,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tint8,Tint8,   Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint8,Tuns8,   Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint8,Tint16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint8,Tuns16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint8,Tint32,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint8,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tint8,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tint8,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tint8,Tint128, Tint128,Tint128, Tint128);
    X(&impCnvTab, Tint8,Tuns128, Tuns128,Tuns128, Tuns128);

    X(&impCnvTab, Tint8,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tint8,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tint8,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tint8,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tint8,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tint8,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tint8,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tint8,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tint8,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tuns8,Tuns8,   Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tuns8,Tint16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tuns8,Tuns16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tuns8,Tint32,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tuns8,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tuns8,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tuns8,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tuns8,Tint128,  Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tuns8,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tuns8,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tuns8,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tuns8,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tuns8,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tuns8,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tuns8,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tuns8,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tuns8,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tuns8,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tint16,Tint16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint16,Tuns16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint16,Tint32,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint16,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tint16,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tint16,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tint16,Tint128,  Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tint16,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tint16,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tint16,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tint16,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tint16,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tint16,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tint16,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tint16,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tint16,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tint16,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tuns16,Tuns16,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tuns16,Tint32,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tuns16,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tuns16,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tuns16,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tuns16,Tint128, Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tuns16,Tuns128, Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tuns16,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tuns16,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tuns16,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tuns16,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tuns16,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tuns16,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tuns16,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tuns16,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tuns16,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tint32,Tint32,  Tint32,Tint32,  Tint32);
    X(&impCnvTab, Tint32,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tint32,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tint32,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tint32,Tint128, Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tint32,Tuns128, Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tint32,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tint32,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tint32,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tint32,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tint32,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tint32,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tint32,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tint32,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tint32,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tuns32,Tuns32,  Tuns32,Tuns32,  Tuns32);
    X(&impCnvTab, Tuns32,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tuns32,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tuns32,Tint128,  Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tuns32,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tuns32,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tuns32,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tuns32,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tuns32,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tuns32,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tuns32,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tuns32,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tuns32,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tuns32,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tint64,Tint64,  Tint64,Tint64,  Tint64);
    X(&impCnvTab, Tint64,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tint64,Tint128,  Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tint64,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tint64,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tint64,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tint64,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tint64,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tint64,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tint64,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tint64,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tint64,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tint64,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tuns64,Tuns64,  Tuns64,Tuns64,  Tuns64);
    X(&impCnvTab, Tuns64,Tint128,  Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tuns64,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tuns64,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tuns64,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tuns64,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tuns64,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tuns64,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tuns64,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tuns64,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tuns64,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tuns64,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tint128,Tint128,  Tint128,Tint128,  Tint128);
    X(&impCnvTab, Tint128,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tint128,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tint128,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tint128,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tint128,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tint128,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tint128,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tint128,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tint128,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tint128,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tuns128,Tuns128,  Tuns128,Tuns128,  Tuns128);

    X(&impCnvTab, Tuns128,Tfloat32,     Tfloat32,Tfloat32,     Tfloat32);
    X(&impCnvTab, Tuns128,Tfloat64,     Tfloat64,Tfloat64,     Tfloat64);
    X(&impCnvTab, Tuns128,Tfloat80,     Tfloat80,Tfloat80,     Tfloat80);
    X(&impCnvTab, Tuns128,Timaginary32, Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tuns128,Timaginary64, Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tuns128,Timaginary80, Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tuns128,Tcomplex32,   Tfloat32,Tcomplex32,   Tcomplex32);
    X(&impCnvTab, Tuns128,Tcomplex64,   Tfloat64,Tcomplex64,   Tcomplex64);
    X(&impCnvTab, Tuns128,Tcomplex80,   Tfloat80,Tcomplex80,   Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tfloat32,Tfloat32,  Tfloat32,Tfloat32, Tfloat32);
    X(&impCnvTab, Tfloat32,Tfloat64,  Tfloat64,Tfloat64, Tfloat64);
    X(&impCnvTab, Tfloat32,Tfloat80,  Tfloat80,Tfloat80, Tfloat80);

    X(&impCnvTab, Tfloat32,Timaginary32,  Tfloat32,Timaginary32, Tfloat32);
    X(&impCnvTab, Tfloat32,Timaginary64,  Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tfloat32,Timaginary80,  Tfloat80,Timaginary80, Tfloat80);

    X(&impCnvTab, Tfloat32,Tcomplex32,  Tfloat32,Tcomplex32, Tcomplex32);
    X(&impCnvTab, Tfloat32,Tcomplex64,  Tfloat64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Tfloat32,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tfloat64,Tfloat64,  Tfloat64,Tfloat64, Tfloat64);
    X(&impCnvTab, Tfloat64,Tfloat80,  Tfloat80,Tfloat80, Tfloat80);

    X(&impCnvTab, Tfloat64,Timaginary32,  Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tfloat64,Timaginary64,  Tfloat64,Timaginary64, Tfloat64);
    X(&impCnvTab, Tfloat64,Timaginary80,  Tfloat80,Timaginary80, Tfloat80);

    X(&impCnvTab, Tfloat64,Tcomplex32,  Tfloat64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Tfloat64,Tcomplex64,  Tfloat64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Tfloat64,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tfloat80,Tfloat80,  Tfloat80,Tfloat80, Tfloat80);

    X(&impCnvTab, Tfloat80,Timaginary32,  Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tfloat80,Timaginary64,  Tfloat80,Timaginary80, Tfloat80);
    X(&impCnvTab, Tfloat80,Timaginary80,  Tfloat80,Timaginary80, Tfloat80);

    X(&impCnvTab, Tfloat80,Tcomplex32,  Tfloat80,Tcomplex80, Tcomplex80);
    X(&impCnvTab, Tfloat80,Tcomplex64,  Tfloat80,Tcomplex80, Tcomplex80);
    X(&impCnvTab, Tfloat80,Tcomplex80,  Tfloat80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Timaginary32,Timaginary32,  Timaginary32,Timaginary32, Timaginary32);
    X(&impCnvTab, Timaginary32,Timaginary64,  Timaginary64,Timaginary64, Timaginary64);
    X(&impCnvTab, Timaginary32,Timaginary80,  Timaginary80,Timaginary80, Timaginary80);

    X(&impCnvTab, Timaginary32,Tcomplex32,  Timaginary32,Tcomplex32, Tcomplex32);
    X(&impCnvTab, Timaginary32,Tcomplex64,  Timaginary64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Timaginary32,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Timaginary64,Timaginary64,  Timaginary64,Timaginary64, Timaginary64);
    X(&impCnvTab, Timaginary64,Timaginary80,  Timaginary80,Timaginary80, Timaginary80);

    X(&impCnvTab, Timaginary64,Tcomplex32,  Timaginary64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Timaginary64,Tcomplex64,  Timaginary64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Timaginary64,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Timaginary80,Timaginary80,  Timaginary80,Timaginary80, Timaginary80);

    X(&impCnvTab, Timaginary80,Tcomplex32,  Timaginary80,Tcomplex80, Tcomplex80);
    X(&impCnvTab, Timaginary80,Tcomplex64,  Timaginary80,Tcomplex80, Tcomplex80);
    X(&impCnvTab, Timaginary80,Tcomplex80,  Timaginary80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tcomplex32,Tcomplex32,  Tcomplex32,Tcomplex32, Tcomplex32);
    X(&impCnvTab, Tcomplex32,Tcomplex64,  Tcomplex64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Tcomplex32,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tcomplex64,Tcomplex64,  Tcomplex64,Tcomplex64, Tcomplex64);
    X(&impCnvTab, Tcomplex64,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80);

    /* ======================= */

    X(&impCnvTab, Tcomplex80,Tcomplex80,  Tcomplex80,Tcomplex80, Tcomplex80);

    foreach (i; 0 .. cast(size_t)TMAX)
    {
        foreach (j; 0 .. cast(size_t)TMAX)
        {
            if (impCnvTab.impcnvResultTab[i][j] == Terror)
            {
                impCnvTab.impcnvResultTab[i][j] = impCnvTab.impcnvResultTab[j][i];
                impCnvTab.impcnvType1Tab[i][j] = impCnvTab.impcnvType2Tab[j][i];
                impCnvTab.impcnvType2Tab[i][j] = impCnvTab.impcnvType1Tab[j][i];
            }
        }
    }

    return impCnvTab;
}
