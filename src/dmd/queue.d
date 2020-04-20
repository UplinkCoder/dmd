module dmd.queue;

version (linux)
{
    version (X86_64)
    {
        alias x86_64_asm_linux_lock = void;
    }
}

static immutable string __mmPause = "asm nothrow pure { rep; nop; }";
immutable string readFence = ``;
immutable string writeFence = ``;
enum debug_threading = 0;

extern(C) uint GetThreadId()
{
    asm nothrow {
        db 0x64,  0x48, 0x8b, 0x0c, 0x25, 0x00, 0x00, 0x00, 0x00;
        // mov RCX, qword ptr FS:0x0; But dmds iasm cannot do that :(
        // rcx = thread_id

        shr RCX, 8;
        mov RAX, RCX;
    }
}

/+
    bool aquire(shared uint aqId) in {
        assert(aqId != 0);
    } body {
        if (threadId == 0) {
            return (&threadId).cas(0, aqId);
        } else if (threadId == aqId) {
            return true;
        } else {
            return false;
        }
    }
+/
__gshared static const(char)* lockedBy;
/+
string QueueIsLocked(string lock)
{
   return 
     (debug_threading) ?
            `(){ bool wasUnlocked = (pthread_spin_trylock(&` ~ lock ~ `) == 0); if (wasUnlocked) pthread_spin_unlock(& ` ~ lock ~ `);printf("(%d): QueueIsLocked: %d\n", __LINE__, !wasUnlocked); return !wasUnlocked; } ()` :   
        `(` ~ lock ~` != 0)`;
}
+/
/// NOTE: I am aware how this function looks,
/// Given the tools I have (buggy iasm) this the best
string LockQueue(string lock)
{
    assert(__ctfe);
    return
        `{enum lbl = mixin("\"L" ~ __LINE__.stringof ~ "\"");
//static assert(is(typeof(` ~ lock ~ `) == shared uint), "can only lock on uint");
    enum lockName = "` ~ lock ~ `";
    auto lockPtr = &` ~lock ~ `;
    static if (debug_threading)
    {
        import core.sys.posix.pthread;
    LtryAgain:
        auto retval = pthread_spin_trylock(lockPtr);
        while(retval != 0) {` ~ __mmPause ~ ` goto LtryAgain;}
        printf("Lock has been locked\n");
        lockedBy = lbl.ptr;
    }
    else
    static if (is(x86_64_asm_linux_lock))
    {
        {
            asm nothrow
            {
                push RAX;
                push RCX;
                push RDX;
                db 0x64,  0x48, 0x8b, 0x0c, 0x25, 0x00, 0x00, 0x00, 0x00;
                // mov RCX, qword ptr FS:0x0; But dmds iasm cannot do that :(
                // rcx = thread_id

                shr RCX, 8;

                // the first byte is always 0 due to alignment
            }
            mixin(lbl ~ ":" ~ "
            asm nothrow {
                xor RAX, RAX; // rax = 0
                mov RDX, [lockPtr]; // load ptr
                lock; cmpxchg dword ptr [RDX], ECX; // xchg()
                // xchg the lock value

                je " ~ lbl ~ ";
                // this code is eqivalent to
                // if (lock == 0) { lock = thread_id; }
                pop RDX;
                pop RCX;
                pop RAX;
            }");
        }
    }
    else
    {
        import core.atomic;
        import core.thread;
/+
        auto t = Thread.getThis();
        const uint id = cast(uint) t.id();
+/
        const shared uint id = GetThreadId();
        uint expected = 0;

        while((*lockPtr) != id && cas((cast(shared)lockPtr), 0, id))
        {
            expected = 0;
        ` ~ __mmPause ~ `
        }
    }

}`;
}

string UnlockQueue(string lock)
{
    static if (debug_threading)
    {
        return `
        auto lockPtr = &` ~ lock ~ `;
        import core.sys.posix.pthread;
        pthread_spin_unlock(lockPtr);
        lockedBy = "Unlocked";
        `;
    }
    else
    return "(*(&" ~ lock ~ ")) = 0;";
}

extern (C)
{
    enum __itt_suppress_all_errors = 0x7fffffff;
    enum __itt_suppress_threading_errors = 0x000000ff;
    enum __itt_suppress_memory_errors = 0x0000ff00;


    enum __itt_suppress_mode
    {
        __itt_unsuppress_range = 0,
        __itt_suppress_range = 1
    }
    enum __itt_unsuppress_range = __itt_suppress_mode.__itt_unsuppress_range;
    enum __itt_suppress_range = __itt_suppress_mode.__itt_suppress_range;


    alias __itt_suppress_mode_t = __itt_suppress_mode;

    /**
    * @brief Mark a range of memory for error suppression or unsuppression for error types included in mask
    */
    alias __itt_suppress_mark_range_t = void function (__itt_suppress_mode_t mode, uint mask, void* address, size_t size);
    alias __itt_api_version_t = char* function ();
}

__gshared static __itt_suppress_mark_range_t __itt_suppress_mark_range;
__gshared static  __itt_api_version_t __itt_api_version;

static if (debug_threading)
{
    __gshared static bool itt_loaded = false;
    __gshared static bool itt_load_attempted = false;

    void loadItt()
    {
        import core.stdc.stdio;
        printf("Calling loadItt()\n");
        if (itt_load_attempted)
            return ;
        itt_load_attempted = true;

        import core.sys.posix.dlfcn;
        import core.stdc.stdlib;
        import core.stdc.string;
        char[255] pathbuf;
        char* inspector_path_prefix = getenv("INSPECTOR_2020_DIR");
        if (inspector_path_prefix)
        {
            strcpy(&pathbuf[0], inspector_path_prefix);
            strcat(&pathbuf[0], "/lib64/runtime/libittnotify.so");
            printf("libpath: %s\n", &pathbuf);

            auto lib = dlopen(&pathbuf[0], RTLD_NOW);
            if (lib)
            {
                __itt_suppress_mark_range = cast(__itt_suppress_mark_range_t) dlsym(lib, "__itt_suppress_mark_range");
                __itt_api_version = cast(__itt_api_version_t) dlsym(lib, "__itt_api_version");
                itt_loaded = true;
            }
            else
            {
                itt_loaded = false;
                printf("loading intel insepector failed ... %s\n", dlerror());
                // another dlerror to clear out the thing
                dlerror();
            }
        }
    }

    void SuppressRace(void* var, size_t var_size)
    {
        if (!itt_load_attempted)
        {
            loadItt();
            import core.stdc.stdio;
        }
        if(itt_loaded)
        {
            __itt_suppress_mark_range(__itt_suppress_range, __itt_suppress_threading_errors, var, var_size);
        } 
    }
}

