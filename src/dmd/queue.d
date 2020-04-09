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

/// NOTE: I am aware how this function looks,
/// Given the tools I have (buggy iasm) this the best
string LockQueue(string lock)
{
    assert(__ctfe);
    return
        `
    static assert(is(typeof(` ~ lock ~ `) == shared uint), "can only lock on uint");
{
    enum lbl = mixin("\"L" ~ __LINE__.stringof ~ "\"");
    enum lockName = "` ~ lock ~ `";
    auto lockPtr = &` ~lock ~ `;
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
    return "(*(&" ~ lock ~ ")) = 0;";
}
