#include "trace.h"

int trace_point_array_count;
_trace_point_* trace_point_array;

#define MAX_EVENTS 65535 * 1024

void init_trace(void)
{
    trace_point_array_count = 0;
    trace_point_array = (_trace_point_*)malloc(MAX_EVENTS * sizeof(_trace_point_));
}
