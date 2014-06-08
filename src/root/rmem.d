
module root.rmem;

import core.stdc.string;

version(GC)
{
import core.memory : GC;

extern(C++)
struct Mem
{
    char* strdup(const char* p)
    {
        return p[0..strlen(p)+1].dup.ptr;
    }
    void free(void* p) {}
    void* malloc(size_t n) { return GC.malloc(n); }
    void* calloc(size_t size, size_t n) { return GC.calloc(size * n); }
    void* realloc(void* p, size_t size) { return GC.realloc(p, size); }
}
extern(C++) __gshared Mem mem;

}
else
{

import core.stdc.stdlib;

extern(C++)
struct Mem
{
    char* strdup(const char* p)
    {
        auto l = strlen(p);
        auto q = cast(char*)malloc(l + 1);
        memcpy(q, p, l + 1);
        return q;
    }
    void free(void* p) {}
    void* malloc(size_t n) { return .malloc(n); }
    void* calloc(size_t size, size_t n) { return .calloc(size, n); }
    void* realloc(void* p, size_t size) { return .realloc(p, size); }
}
extern(C++) __gshared Mem mem;

}
