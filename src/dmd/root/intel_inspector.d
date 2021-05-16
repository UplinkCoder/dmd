module dmd.root.intel_inspector;

import core.sys.posix.dlfcn;
import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;

extern (C) @nogc nothrow __gshared {
    void dummy (void *p) {}

/**
 * @brief Quit spin loop without acquiring spin object
 */
    void function (void* addr) __itt_sync_cancel = &dummy;
/**
 * @brief Successful spin loop completion (sync object acquired)
 */
    void function (void* addr) __itt_sync_acquired = &dummy;
/**
 * @brief Start sync object releasing code. Is called before the lock release call.
 */
    void function (void* addr) __itt_sync_releasing = &dummy;
}

shared static this()
{
    auto insp_dir_c = getenv("INSPECTOR_2021_DIR");
    auto insp_dir = insp_dir_c ? cast(string) insp_dir_c[0 .. strlen(insp_dir_c)] : null;
    if (insp_dir)
    {
        char[1024] pathbuf;
        sprintf(pathbuf.ptr, "%s/lib64/runtime/libittnotify.so", insp_dir_c);
        auto lib = dlopen(pathbuf.ptr, RTLD_NOW);
        if (lib)
        {
            __itt_sync_cancel = cast(typeof(__itt_sync_cancel))dlsym(lib, "__itt_sync_cancel");
            __itt_sync_acquired = cast(typeof(__itt_sync_acquired))dlsym(lib, "__itt_sync_acquired");
            __itt_sync_releasing = cast(typeof(__itt_sync_releasing))dlsym(lib, "__itt_sync_releasing");

            return ;
        }
    }

    fprintf(stderr, "intel inspector library functions could not be loaded\n");
}
