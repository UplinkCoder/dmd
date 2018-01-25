#ifndef _TRACE_H_
#define _TRACE_H_

#include <x86intrin.h>
#include <stdint.h>
#include "dsymbol.h"

#define TRACE(sym) \
    _trace_point_ _tp = _trace_point_(sym, __FUNCTION__);


struct _trace_point_;

extern int trace_point_array_count;
extern _trace_point_* trace_point_array;

struct _trace_point_
{
    uint64_t begin_ticks;
    uint64_t end_ticks;
    Dsymbol *sym;
    const char* name;
    int trace_point_index;


    _trace_point_(Dsymbol* _sym, const char* _name)
    {
        begin_ticks = __rdtsc();
        name = _name;
        sym = _sym;
        trace_point_index = trace_point_array_count++;
    }

    ~_trace_point_()
    {
        end_ticks = __rdtsc();
        trace_point_array[trace_point_index] = *this;
    }
};


void init_trace(void);

#endif //_TRACE_H_
