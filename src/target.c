
/* Compiler implementation of the D programming language
 * Copyright (c) 2013-2014 by Digital Mars
 * All Rights Reserved
 * written by Iain Buclaw
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/target.c
 */

#include <assert.h>

#include "target.h"
#include "mars.h"
#include "mtype.h"

int Target::ptrsize;
int Target::realsize;
int Target::realpad;
int Target::realalignsize;
bool Target::reverseCppOverloads;
int Target::longsize;


void Target::init()
{
    // These have default values for 32 bit code, they get
    // adjusted for 64 bit code.
    ptrsize = 4;

    if (global.params.isLinux || global.params.isFreeBSD
        || global.params.isOpenBSD || global.params.isSolaris)
    {
        realsize = 12;
        realpad = 2;
        realalignsize = 4;
        longsize = 4;
    }
    else if (global.params.isOSX)
    {
        realsize = 16;
        realpad = 6;
        realalignsize = 16;
        longsize = 4;
    }
    else if (global.params.isWindows)
    {
        realsize = 10;
        realpad = 0;
        realalignsize = 2;
        reverseCppOverloads = !global.params.is64bit;
        longsize = 4;
    }
    else
        assert(0);

    if (global.params.is64bit)
    {
        if (global.params.isLinux || global.params.isFreeBSD || global.params.isSolaris)
        {
            realsize = 16;
            realpad = 6;
            realalignsize = 16;
            longsize = 8;
        }
        else if (global.params.isOSX)
        {
            longsize = 8;
        }
    }

    if (global.params.isLP64)
        ptrsize = 8;
}

/******************************
 * Return memory alignment size of type.
 */

unsigned Target::alignsize(Type* type)
{
    assert (type->isTypeBasic());

    switch (type->ty)
    {
        case Tfloat80:
        case Timaginary80:
        case Tcomplex80:
            return Target::realalignsize;

        case Tcomplex32:
            if (global.params.isLinux || global.params.isOSX || global.params.isFreeBSD
                || global.params.isOpenBSD || global.params.isSolaris)
                return 4;
            break;

        case Tint64:
        case Tuns64:
        case Tfloat64:
        case Timaginary64:
        case Tcomplex64:
            if (global.params.isLinux || global.params.isOSX || global.params.isFreeBSD
                || global.params.isOpenBSD || global.params.isSolaris)
                return global.params.is64bit ? 8 : 4;
            break;

        default:
            break;
    }
    return (unsigned)type->size(Loc());
}

/******************************
 * Return field alignment size of type.
 */

unsigned Target::fieldalign(Type* type)
{
    return type->alignsize();
}

/***********************************
 * Return size of OS critical section.
 * NOTE: can't use the sizeof() calls directly since cross compiling is
 * supported and would end up using the host sizes rather than the target
 * sizes.
 */
unsigned Target::critsecsize()
{
    if (global.params.isWindows)
    {
        // sizeof(CRITICAL_SECTION) for Windows.
        return global.params.isLP64 ? 40 : 24;
    }
    else if (global.params.isLinux)
    {
        // sizeof(pthread_mutex_t) for Linux.
        if (global.params.is64bit)
            return global.params.isLP64 ? 40 : 32;
        else
            return global.params.isLP64 ? 40 : 24;
    }
    else if (global.params.isFreeBSD)
    {
        // sizeof(pthread_mutex_t) for FreeBSD.
        return global.params.isLP64 ? 8 : 4;
    }
    else if (global.params.isOpenBSD)
    {
        // sizeof(pthread_mutex_t) for OpenBSD.
        return global.params.isLP64 ? 8 : 4;
    }
    else if (global.params.isOSX)
    {
        // sizeof(pthread_mutex_t) for OSX.
        return global.params.isLP64 ? 64 : 44;
    }
    else if (global.params.isSolaris)
    {
        // sizeof(pthread_mutex_t) for Solaris.
        return 24;
    }
    assert(0);
    return 0;
}

/***********************************
 * Returns a Type for the va_list type of the target.
 * NOTE: For Posix/x86_64 this returns the type which will really
 * be used for passing an argument of type va_list.
 */
Type *Target::va_listType()
{
    if (global.params.isWindows)
    {
        return Type::tchar->pointerTo();
    }
    else if (global.params.isLinux ||
             global.params.isFreeBSD ||
             global.params.isOpenBSD ||
             global.params.isSolaris ||
             global.params.isOSX)
    {
        if (global.params.is64bit)
        {
            return (new TypeIdentifier(Loc(), Lexer::idPool("__va_list_tag")))->pointerTo();
        }
        else
        {
            return Type::tchar->pointerTo();
        }
    }
    else
    {
        assert(0);
        return NULL;
    }
}

/******************************
 * Encode the given expression, which is assumed to be an rvalue literal
 * as another type for use in CTFE.
 * This corresponds roughly to the idiom *(Type *)&e.
 */

Expression *Target::paintAsType(Expression *e, Type *type)
{
    union
    {
        d_int32 int32value;
        d_int64 int64value;
        float float32value;
        double float64value;
    } u;

    assert(e->type->size() == type->size());

    switch (e->type->ty)
    {
        case Tint32:
        case Tuns32:
            u.int32value = (d_int32)e->toInteger();
            break;

        case Tint64:
        case Tuns64:
            u.int64value = (d_int64)e->toInteger();
            break;

        case Tfloat32:
            u.float32value = e->toReal();
            break;

        case Tfloat64:
            u.float64value = e->toReal();
            break;

        default:
            assert(0);
    }

    switch (type->ty)
    {
        case Tint32:
        case Tuns32:
            return new IntegerExp(e->loc, u.int32value, type);

        case Tint64:
        case Tuns64:
            return new IntegerExp(e->loc, u.int64value, type);

        case Tfloat32:
            return new RealExp(e->loc, ldouble(u.float32value), type);

        case Tfloat64:
            return new RealExp(e->loc, ldouble(u.float64value), type);

        default:
            assert(0);
    }

    return NULL;    // avoid warning
}
