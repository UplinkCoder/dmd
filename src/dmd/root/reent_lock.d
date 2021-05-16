module dmd.root.reent_lock;

import core.atomic;

import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;
import core.sys.posix.dlfcn;

import dmd.root.intel_inspector;

shared void* NULL = null;


// FIXME: I AM SURE THIS IS BROKEN
/// Reenterant Lock syncronisation mechanism
struct ReentLock
{
@nogc: nothrow:
    shared align(16) void* current_owner;
    shared align(16) uint lock_count;

    Loc lastAquiredLoc;
    struct Loc
    {
        string func;
        string file;
        int line;
    }

    void lock(void* owner, string func = __FUNCTION__, string file = __FILE__, int line = __LINE__) shared
    {
        pragma(inline, true);
        while(current_owner !is null && current_owner !is owner) {}

        if (current_owner is owner)
        {
            atomicFetchAdd(lock_count, 1);
        }
        if (cas(&this.current_owner, NULL, cast(shared)owner))
        {
            lastAquiredLoc = Loc(func, file, line);
            __itt_sync_acquired(cast(void*) &this);
        }
    }

    void unlock(void* owner) shared
    {
        pragma(inline, true);
        if (owner == current_owner)
        {
            atomicFetchAdd(lock_count, -1);
            if (atomicLoad(lock_count) == 0)
            {
                __itt_sync_releasing(cast(void*) &this);
                lastAquiredLoc = Loc.init;
                assert(cas(&this.current_owner, cast(shared)owner, null));
            }
        }
    }
}
