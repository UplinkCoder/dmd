/**
 * The fiber module provides OS-indepedent lightweight threads aka fibers.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/fiber.d)
 */

module dmd.myFiber_ldc;

import core.memory;
const PAGESIZE = 4096;

import core.thread.osthread;
pragma(msg, __traits(allMembers, Thread));
pragma(msg, Thread.Context);
version (LDC)
{
    import ldc.attributes;
    import ldc.llvmasm;

    version (SupportSanitizers)
    {
        import ldc.sanitizers_optionally_linked;
    }
}

///////////////////////////////////////////////////////////////////////////////
// Fiber Platform Detection
///////////////////////////////////////////////////////////////////////////////

version (GNU)
{
    import gcc.builtins;
    version (GNU_StackGrowsDown)
        version = StackGrowsDown;
}
else
{
    // this should be true for most architectures
    version = StackGrowsDown;
}

version (Windows)
{
    import core.stdc.stdlib : malloc, free;
}

private
{
    version (D_InlineAsm_X86)
    {
        version (Windows)
            version = AsmX86_Windows;
        else version (Posix)
            version = AsmX86_Posix;

        version = AlignFiberStackTo16Byte;
    }
    else version (D_InlineAsm_X86_64)
    {
        version (Windows)
        {
            version = AsmX86_64_Windows;
            version = AlignFiberStackTo16Byte;
        }
        else version (Posix)
        {
            version = AsmX86_64_Posix;
            version = AlignFiberStackTo16Byte;
        }
    }
    else version (PPC)
    {
        version (Posix)
        {
            version = AsmPPC_Posix;
            version = AsmExternal;
        }
    }
    else version (PPC64)
    {
        version (Posix)
        {
            version = AsmPPC64_Posix;
            version = AsmExternal;
            version = AlignFiberStackTo16Byte;
        }
    }
    else version (MIPS_O32)
    {
        version (Posix)
        {
            version = AsmMIPS_O32_Posix;
            version = AsmExternal;
        }
    }
    else version (AArch64)
    {
        version (Posix)
        {
            version = AsmAArch64_Posix;
            version = AsmExternal;
            version = AlignFiberStackTo16Byte;
        }
    }
    else version (ARM)
    {
        version (Posix)
        {
            version = AsmARM_Posix;
            version = AsmExternal;
        }
    }
    else version (SPARC)
    {
        // NOTE: The SPARC ABI specifies only doubleword alignment.
        version = AlignFiberStackTo16Byte;
    }
    else version (SPARC64)
    {
        version = AlignFiberStackTo16Byte;
    }

    version (Posix)
    {
        import core.sys.posix.unistd;   // for sysconf

        version (AsmX86_Windows)    {} else
        version (AsmX86_Posix)      {} else
        version (AsmX86_64_Windows) {} else
        version (AsmX86_64_Posix)   {} else
        version (AsmExternal)       {} else
        {
            // NOTE: The ucontext implementation requires architecture specific
            //       data definitions to operate so testing for it must be done
            //       by checking for the existence of ucontext_t rather than by
            //       a version identifier.  Please note that this is considered
            //       an obsolescent feature according to the POSIX spec, so a
            //       custom solution is still preferred.
            import core.sys.posix.ucontext;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// Fiber Entry Point and Context Switch
///////////////////////////////////////////////////////////////////////////////

private
{
    import core.atomic : atomicStore, cas, MemoryOrder;
    import core.exception : onOutOfMemoryError;
    import core.stdc.stdlib : abort;

    extern (C) void fiber_entryPoint() nothrow /* LDC */ @assumeUsed
    {
        Fiber   obj = Fiber.getThis();
        assert( obj );

        version (SupportSanitizers)
        {
            informSanitizerOfFinishSwitchFiber(obj.__fake_stack, &obj.__from_stack_bottom, &obj.__from_stack_size);
        }

        assert( Thread.getThis().m_curr is obj.m_ctxt );
        atomicStore!(MemoryOrder.raw)(*cast(shared)&Thread.getThis().m_lock, false);
        obj.m_ctxt.tstack = obj.m_ctxt.bstack;
        obj.m_state = Fiber.State.EXEC;

        try
        {
            obj.run();
        }
        catch ( Throwable t )
        {
            obj.m_unhandled = t;
        }

        static if ( __traits( compiles, ucontext_t ) )
          obj.m_ucur = &obj.m_utxt;

        obj.m_state = Fiber.State.TERM;
        obj.switchOut();
    }

  // Look above the definition of 'class Fiber' for some information about the implementation of this routine
  version (AsmExternal)
  {
      extern (C) void fiber_switchContext( void** oldp, void* newp ) nothrow @nogc;
      version (AArch64)
          extern (C) void fiber_trampoline() nothrow;
  }
  else version (LDC_Windows)
  {
    extern (C) void fiber_switchContext( void** oldp, void* newp ) nothrow @nogc @naked
    {
        version (X86)
        {
            pragma(LDC_never_inline);

            __asm(
               `// save current stack state
                push %ebp
                mov  %esp, %ebp
                push %edi
                push %esi
                push %ebx
                push %fs:(0)
                push %fs:(4)
                push %fs:(8)
                push %eax

                // store oldp again with more accurate address
                mov 8(%ebp), %eax
                mov %esp, (%eax)
                // load newp to begin context switch
                mov 12(%ebp), %esp

                // load saved state from new stack
                pop %eax
                pop %fs:(8)
                pop %fs:(4)
                pop %fs:(0)
                pop %ebx
                pop %esi
                pop %edi
                pop %ebp

                // 'return' to complete switch
                pop %ecx
                jmp *%ecx`,
                "~{memory},~{ebp},~{esp},~{eax},~{ebx},~{ecx},~{esi},~{edi}"
            );
        }
        else version (X86_64)
        {
            // This inline asm assumes a return address has been pushed onto the stack
            // (and so a stack not aligned to 16 bytes).
            pragma(LDC_never_inline);

            __asm(
               `// save current stack state
                push %rbp
                mov  %rsp, %rbp
                push %r12
                push %r13
                push %r14
                push %r15
                push %rdi
                push %rsi
                // 7 registers = 56 bytes; stack is now aligned to 16 bytes
                sub $$0xA0, %rsp
                movdqa %xmm6, 0x90(%rsp)
                movdqa %xmm7, 0x80(%rsp)
                movdqa %xmm8, 0x70(%rsp)
                movdqa %xmm9, 0x60(%rsp)
                movdqa %xmm10, 0x50(%rsp)
                movdqa %xmm11, 0x40(%rsp)
                movdqa %xmm12, 0x30(%rsp)
                movdqa %xmm13, 0x20(%rsp)
                movdqa %xmm14, 0x10(%rsp)
                movdqa %xmm15, (%rsp)
                push %rbx
                xor  %rax, %rax
                push %gs:(%rax)
                push %gs:8(%rax)
                push %gs:16(%rax)

                // store oldp
                mov %rsp, (%rcx)
                // load newp to begin context switch
                mov %rdx, %rsp

                // load saved state from new stack
                pop %gs:16(%rax)
                pop %gs:8(%rax)
                pop %gs:(%rax)
                pop %rbx;
                movdqa (%rsp), %xmm15
                movdqa 0x10(%rsp), %xmm14
                movdqa 0x20(%rsp), %xmm13
                movdqa 0x30(%rsp), %xmm12
                movdqa 0x40(%rsp), %xmm11
                movdqa 0x50(%rsp), %xmm10
                movdqa 0x60(%rsp), %xmm9
                movdqa 0x70(%rsp), %xmm8
                movdqa 0x80(%rsp), %xmm7
                movdqa 0x90(%rsp), %xmm6
                add $$0xA0, %rsp
                pop %rsi
                pop %rdi
                pop %r15
                pop %r14
                pop %r13
                pop %r12
                pop %rbp

                // 'return' to complete switch
                pop %rcx
                jmp *%rcx`,
                "~{memory},~{rbp},~{rsp},~{rax},~{rbx},~{rcx},~{rsi},~{rdi},~{r12},~{r13},~{r14},~{r15}," ~
                "~{xmm6},~{xmm7},~{xmm8},~{xmm9},~{xmm10},~{xmm11},~{xmm12},~{xmm13},~{xmm14},~{xmm15}"
            );
        }
        else
            static assert(false);
    }
  }
  else
    extern (C) void fiber_switchContext( void** oldp, void* newp ) nothrow @nogc
    {
        // NOTE: The data pushed and popped in this routine must match the
        //       default stack created by Fiber.initStack or the initial
        //       switch into a new context will fail.

        version (AsmX86_Windows)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                push EBP;
                mov  EBP, ESP;
                push EDI;
                push ESI;
                push EBX;
                push dword ptr FS:[0];
                push dword ptr FS:[4];
                push dword ptr FS:[8];
                push EAX;

                // store oldp again with more accurate address
                mov EAX, dword ptr 8[EBP];
                mov [EAX], ESP;
                // load newp to begin context switch
                mov ESP, dword ptr 12[EBP];

                // load saved state from new stack
                pop EAX;
                pop dword ptr FS:[8];
                pop dword ptr FS:[4];
                pop dword ptr FS:[0];
                pop EBX;
                pop ESI;
                pop EDI;
                pop EBP;

                // 'return' to complete switch
                pop ECX;
                jmp ECX;
            }
        }
        else version (AsmX86_64_Windows)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                // NOTE: When changing the layout of registers on the stack,
                //       make sure that the XMM registers are still aligned.
                //       On function entry, the stack is guaranteed to not
                //       be aligned to 16 bytes because of the return address
                //       on the stack.
                push RBP;
                mov  RBP, RSP;
                push R12;
                push R13;
                push R14;
                push R15;
                push RDI;
                push RSI;
                // 7 registers = 56 bytes; stack is now aligned to 16 bytes
                sub RSP, 160;
                movdqa [RSP + 144], XMM6;
                movdqa [RSP + 128], XMM7;
                movdqa [RSP + 112], XMM8;
                movdqa [RSP + 96], XMM9;
                movdqa [RSP + 80], XMM10;
                movdqa [RSP + 64], XMM11;
                movdqa [RSP + 48], XMM12;
                movdqa [RSP + 32], XMM13;
                movdqa [RSP + 16], XMM14;
                movdqa [RSP], XMM15;
                push RBX;
                xor  RAX,RAX;
                push qword ptr GS:[RAX];
                push qword ptr GS:8[RAX];
                push qword ptr GS:16[RAX];

                // store oldp
                mov [RCX], RSP;
                // load newp to begin context switch
                mov RSP, RDX;

                // load saved state from new stack
                pop qword ptr GS:16[RAX];
                pop qword ptr GS:8[RAX];
                pop qword ptr GS:[RAX];
                pop RBX;
                movdqa XMM15, [RSP];
                movdqa XMM14, [RSP + 16];
                movdqa XMM13, [RSP + 32];
                movdqa XMM12, [RSP + 48];
                movdqa XMM11, [RSP + 64];
                movdqa XMM10, [RSP + 80];
                movdqa XMM9, [RSP + 96];
                movdqa XMM8, [RSP + 112];
                movdqa XMM7, [RSP + 128];
                movdqa XMM6, [RSP + 144];
                add RSP, 160;
                pop RSI;
                pop RDI;
                pop R15;
                pop R14;
                pop R13;
                pop R12;
                pop RBP;

                // 'return' to complete switch
                pop RCX;
                jmp RCX;
            }
        }
        else version (AsmX86_Posix)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                push EBP;
                mov  EBP, ESP;
                push EDI;
                push ESI;
                push EBX;
                push EAX;

                // store oldp again with more accurate address
                mov EAX, dword ptr 8[EBP];
                mov [EAX], ESP;
                // load newp to begin context switch
                mov ESP, dword ptr 12[EBP];

                // load saved state from new stack
                pop EAX;
                pop EBX;
                pop ESI;
                pop EDI;
                pop EBP;

                // 'return' to complete switch
                pop ECX;
                jmp ECX;
            }
        }
        else version (AsmX86_64_Posix)
        {
            asm pure nothrow @nogc
            {
                naked;

                // save current stack state
                push RBP;
                mov  RBP, RSP;
                push RBX;
                push R12;
                push R13;
                push R14;
                push R15;

                // store oldp
                mov [RDI], RSP;
                // load newp to begin context switch
                mov RSP, RSI;

                // load saved state from new stack
                pop R15;
                pop R14;
                pop R13;
                pop R12;
                pop RBX;
                pop RBP;

                // 'return' to complete switch
                pop RCX;
                jmp RCX;
            }
        }
        else static if ( __traits( compiles, ucontext_t ) )
        {
            Fiber   cfib = Fiber.getThis();
            void*   ucur = cfib.m_ucur;

            *oldp = &ucur;
            swapcontext( **(cast(ucontext_t***) oldp),
                          *(cast(ucontext_t**)  newp) );
        }
        else
            static assert(0, "Not implemented");
    }
}

// Detect when a Fiber migrates between Threads for systems in which it may be
// unsafe to do so.  A unittest below helps decide if CheckFiberMigration
// should be set for your system.

version (LDC)
{
    version (OSX)
    {
        version (ARM) version = CheckFiberMigration;
        version (AArch64) version = CheckFiberMigration;
        version (X86) version = CheckFiberMigration;
        version (X86_64) version = CheckFiberMigration;
    }

    version (Android) version = CheckFiberMigration;
}

// Fiber support for SjLj style exceptions
//
// Exception handling based on setjmp/longjmp tracks the unwind points with a
// linked list stack managed by _Unwind_SjLj_Register and
// _Unwind_SjLj_Unregister.  In the context of Fibers, the stack needs to be
// Fiber local, otherwise unwinding could weave through functions on other
// Fibers as opposed to just the current Fiber.  The solution is to give each
// Fiber a m_sjljExStackTop.
//
// Two implementations known to have this SjLj stack design are GCC's libgcc
// and darwin libunwind for ARM (iOS).  Functions to get/set the current SjLj
// stack are named differently in each implmentation:
//
// https://github.com/gcc-mirror/gcc/blob/master/libgcc/unwind-sjlj.c
//
// libgcc
//   struct SjLj_Function_Context* _Unwind_SjLj_GetContext(void)
//   void _Unwind_SjLj_SetContext(struct SjLj_Function_Context *fc)
//
// http://www.opensource.apple.com/source/libunwind/libunwind-30/src/Unwind-sjlj.c
//
// darwin (OS X)
//   _Unwind_FunctionContext* __Unwind_SjLj_GetTopOfFunctionStack();
//   void __Unwind_SjLj_SetTopOfFunctionStack(_Unwind_FunctionContext* fc);
//
// These functions are not extern but if we peek at the implementations it
// turns out that _Unwind_SjLj_Register and _Unwind_SjLj_Unregister in both
// libraries will manipulate the stack as we need.

version (GNU_SjLj_Exceptions) version = SjLj_Exceptions;
version (iOS) version(ARM)    version = SjLj_Exceptions;

version (SjLj_Exceptions)
private
{
    // libgcc struct SjLj_Function_Context and darwin struct
    // _Unwind_FunctionContext have same initial layout so can get away with
    // one type to mimic header of both here.
    struct SjLjFuncContext
    {
        SjLjFuncContext* prev;
        // rest of this struc we don't care about in swapSjLjStackTop below.
    }

    extern(C) @nogc nothrow
    {
        void _Unwind_SjLj_Register(SjLjFuncContext* fc);
        void _Unwind_SjLj_Unregister(SjLjFuncContext* fc);
    }

    // Swap in a new stack top, returning the previous one
    SjLjFuncContext* swapSjLjStackTop(SjLjFuncContext* newtop) @nogc nothrow
    {
        // register a dummy context to retrieve stack top, then plop our new
        // stack top in its place before unregistering, making it the new top.
        SjLjFuncContext fc;
        _Unwind_SjLj_Register(&fc);

        SjLjFuncContext* prevtop = fc.prev;
        fc.prev = newtop;
        _Unwind_SjLj_Unregister(&fc);

        return prevtop;
    }
}

///////////////////////////////////////////////////////////////////////////////
// Fiber
///////////////////////////////////////////////////////////////////////////////
/*
 * Documentation of Fiber internals:
 *
 * The main routines to implement when porting Fibers to new architectures are
 * fiber_switchContext and initStack. Some version constants have to be defined
 * for the new platform as well, search for "Fiber Platform Detection and Memory Allocation".
 *
 * Fibers are based on a concept called 'Context'. A Context describes the execution
 * state of a Fiber or main thread which is fully described by the stack, some
 * registers and a return address at which the Fiber/Thread should continue executing.
 * Please note that not only each Fiber has a Context, but each thread also has got a
 * Context which describes the threads stack and state. If you call Fiber fib; fib.call
 * the first time in a thread you switch from Threads Context into the Fibers Context.
 * If you call fib.yield in that Fiber you switch out of the Fibers context and back
 * into the Thread Context. (However, this is not always the case. You can call a Fiber
 * from within another Fiber, then you switch Contexts between the Fibers and the Thread
 * Context is not involved)
 *
 * In all current implementations the registers and the return address are actually
 * saved on a Contexts stack.
 *
 * The fiber_switchContext routine has got two parameters:
 * void** a:  This is the _location_ where we have to store the current stack pointer,
 *            the stack pointer of the currently executing Context (Fiber or Thread).
 * void*  b:  This is the pointer to the stack of the Context which we want to switch into.
 *            Note that we get the same pointer here as the one we stored into the void** a
 *            in a previous call to fiber_switchContext.
 *
 * In the simplest case, a fiber_switchContext rountine looks like this:
 * fiber_switchContext:
 *     push {return Address}
 *     push {registers}
 *     copy {stack pointer} into {location pointed to by a}
 *     //We have now switch to the stack of a different Context!
 *     copy {b} into {stack pointer}
 *     pop {registers}
 *     pop {return Address}
 *     jump to {return Address}
 *
 * The GC uses the value returned in parameter a to scan the Fibers stack. It scans from
 * the stack base to that value. As the GC dislikes false pointers we can actually optimize
 * this a little: By storing registers which can not contain references to memory managed
 * by the GC outside of the region marked by the stack base pointer and the stack pointer
 * saved in fiber_switchContext we can prevent the GC from scanning them.
 * Such registers are usually floating point registers and the return address. In order to
 * implement this, we return a modified stack pointer from fiber_switchContext. However,
 * we have to remember that when we restore the registers from the stack!
 *
 * --------------------------- <= Stack Base
 * |          Frame          | <= Many other stack frames
 * |          Frame          |
 * |-------------------------| <= The last stack frame. This one is created by fiber_switchContext
 * | registers with pointers |
 * |                         | <= Stack pointer. GC stops scanning here
 * |   return address        |
 * |floating point registers |
 * --------------------------- <= Real Stack End
 *
 * fiber_switchContext:
 *     push {registers with pointers}
 *     copy {stack pointer} into {location pointed to by a}
 *     push {return Address}
 *     push {Floating point registers}
 *     //We have now switch to the stack of a different Context!
 *     copy {b} into {stack pointer}
 *     //We now have to adjust the stack pointer to point to 'Real Stack End' so we can pop
 *     //the FP registers
 *     //+ or - depends on if your stack grows downwards or upwards
 *     {stack pointer} = {stack pointer} +- ({FPRegisters}.sizeof + {return address}.sizeof}
 *     pop {Floating point registers}
 *     pop {return Address}
 *     pop {registers with pointers}
 *     jump to {return Address}
 *
 * So the question now is which registers need to be saved? This depends on the specific
 * architecture ABI of course, but here are some general guidelines:
 * - If a register is callee-save (if the callee modifies the register it must saved and
 *   restored by the callee) it needs to be saved/restored in switchContext
 * - If a register is caller-save it needn't be saved/restored. (Calling fiber_switchContext
 *   is a function call and the compiler therefore already must save these registers before
 *   calling fiber_switchContext)
 * - Argument registers used for passing parameters to functions needn't be saved/restored
 * - The return register needn't be saved/restored (fiber_switchContext hasn't got a return type)
 * - All scratch registers needn't be saved/restored
 * - The link register usually needn't be saved/restored (but sometimes it must be cleared -
 *   see below for details)
 * - The frame pointer register - if it exists - is usually callee-save
 * - All current implementations do not save control registers
 *
 * What happens on the first switch into a Fiber? We never saved a state for this fiber before,
 * but the initial state is prepared in the initStack routine. (This routine will also be called
 * when a Fiber is being resetted). initStack must produce exactly the same stack layout as the
 * part of fiber_switchContext which saves the registers. Pay special attention to set the stack
 * pointer correctly if you use the GC optimization mentioned before. the return Address saved in
 * initStack must be the address of fiber_entrypoint.
 *
 * There's now a small but important difference between the first context switch into a fiber and
 * further context switches. On the first switch, Fiber.call is used and the returnAddress in
 * fiber_switchContext will point to fiber_entrypoint. The important thing here is that this jump
 * is a _function call_, we call fiber_entrypoint by jumping before it's function prologue. On later
 * calls, the user used yield() in a function, and therefore the return address points into a user
 * function, after the yield call. So here the jump in fiber_switchContext is a _function return_,
 * not a function call!
 *
 * The most important result of this is that on entering a function, i.e. fiber_entrypoint, we
 * would have to provide a return address / set the link register once fiber_entrypoint
 * returns. Now fiber_entrypoint does never return and therefore the actual value of the return
 * address / link register is never read/used and therefore doesn't matter. When fiber_switchContext
 * performs a _function return_ the value in the link register doesn't matter either.
 * However, the link register will still be saved to the stack in fiber_entrypoint and some
 * exception handling / stack unwinding code might read it from this stack location and crash.
 * The exact solution depends on your architecture, but see the ARM implementation for a way
 * to deal with this issue.
 *
 * The ARM implementation is meant to be used as a kind of documented example implementation.
 * Look there for a concrete example.
 *
 * FIXME: fiber_entrypoint might benefit from a @noreturn attribute, but D doesn't have one.
 */

/**
 * This class provides a cooperative concurrency mechanism integrated with the
 * threading and garbage collection functionality.  Calling a fiber may be
 * considered a blocking operation that returns when the fiber yields (via
 * Fiber.yield()).  Execution occurs within the context of the calling thread
 * so synchronization is not necessary to guarantee memory visibility so long
 * as the same thread calls the fiber each time.  Please note that there is no
 * requirement that a fiber be bound to one specific thread.  Rather, fibers
 * may be freely passed between threads so long as they are not currently
 * executing.  Like threads, a new fiber thread may be created using either
 * derivation or composition, as in the following example.
 *
 * Warning:
 * Status registers are not saved by the current implementations. This means
 * floating point exception status bits (overflow, divide by 0), rounding mode
 * and similar stuff is set per-thread, not per Fiber!
 *
 * Warning:
 * On ARM FPU registers are not saved if druntime was compiled as ARM_SoftFloat.
 * If such a build is used on a ARM_SoftFP system which actually has got a FPU
 * and other libraries are using the FPU registers (other code is compiled
 * as ARM_SoftFP) this can cause problems. Druntime must be compiled as
 * ARM_SoftFP in this case.
 *
 * Authors: Based on a design by Mikola Lysenko.
 */
class Fiber
{
    ///////////////////////////////////////////////////////////////////////////
    // Initialization
    ///////////////////////////////////////////////////////////////////////////

    version (Windows)
        // exception handling walks the stack, invoking DbgHelp.dll which
        // needs up to 16k of stack space depending on the version of DbgHelp.dll,
        // the existence of debug symbols and other conditions. Avoid causing
        // stack overflows by defaulting to a larger stack size
        enum defaultStackPages = 8;
    else
        enum defaultStackPages = 4;

    /**
     * Initializes a fiber object which is associated with a static
     * D function.
     *
     * Params:
     *  fn = The fiber function.
     *  sz = The stack size for this fiber.
     *  guardPageSize = size of the guard page to trap fiber's stack
     *                  overflows. Beware that using this will increase
     *                  the number of mmaped regions on platforms using mmap
     *                  so an OS-imposed limit may be hit.
     *
     * In:
     *  fn must not be null.
     */
    this( void function() fn, size_t sz = PAGESIZE * defaultStackPages,
          size_t guardPageSize = PAGESIZE ) nothrow
    in
    {
        assert( fn );
    }
    do
    {
        allocStack( sz, guardPageSize );
        reset( fn );
    }


    /**
     * Initializes a fiber object which is associated with a dynamic
     * D function.
     *
     * Params:
     *  dg = The fiber function.
     *  sz = The stack size for this fiber.
     *  guardPageSize = size of the guard page to trap fiber's stack
     *                  overflows. Beware that using this will increase
     *                  the number of mmaped regions on platforms using mmap
     *                  so an OS-imposed limit may be hit.
     *
     * In:
     *  dg must not be null.
     */
    this( void delegate() dg, size_t sz = PAGESIZE * defaultStackPages,
          size_t guardPageSize = PAGESIZE ) nothrow
    in
    {
        assert( dg );
    }
    do
    {
        allocStack( sz, guardPageSize );
        reset( dg );
    }


    /**
     * Cleans up any remaining resources used by this object.
     */
    ~this() nothrow @nogc
    {
        // NOTE: A live reference to this object will exist on its associated
        //       stack from the first time its call() method has been called
        //       until its execution completes with State.TERM.  Thus, the only
        //       times this dtor should be called are either if the fiber has
        //       terminated (and therefore has no active stack) or if the user
        //       explicitly deletes this object.  The latter case is an error
        //       but is not easily tested for, since State.HOLD may imply that
        //       the fiber was just created but has never been run.  There is
        //       not a compelling case to create a State.INIT just to offer a
        //       means of ensuring the user isn't violating this object's
        //       contract, so for now this requirement will be enforced by
        //       documentation only.
        freeStack();
    }


    ///////////////////////////////////////////////////////////////////////////
    // General Actions
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Transfers execution to this fiber object.  The calling context will be
     * suspended until the fiber calls Fiber.yield() or until it terminates
     * via an unhandled exception.
     *
     * Params:
     *  rethrow = Rethrow any unhandled exception which may have caused this
     *            fiber to terminate.
     *
     * In:
     *  This fiber must be in state HOLD.
     *
     * Throws:
     *  Any exception not handled by the joined thread.
     *
     * Returns:
     *  Any exception not handled by this fiber if rethrow = false, null
     *  otherwise.
     */
    // Not marked with any attributes, even though `nothrow @nogc` works
    // because it calls arbitrary user code. Most of the implementation
    // is already `@nogc nothrow`, but in order for `Fiber.call` to
    // propagate the attributes of the user's function, the Fiber
    // class needs to be templated.
    final Throwable call( Rethrow rethrow = Rethrow.yes )
    {
        return rethrow ? call!(Rethrow.yes)() : call!(Rethrow.no);
    }

    /// ditto
    final Throwable call( Rethrow rethrow )()
    {
        callImpl();
        if ( m_unhandled )
        {
            Throwable t = m_unhandled;
            m_unhandled = null;
            static if ( rethrow )
                throw t;
            else
                return t;
        }
        return null;
    }

    private void callImpl() nothrow @nogc
    in
    {
        assert( m_state == State.HOLD );
    }
    do
    {
        Fiber   cur = getThis();

        static if ( __traits( compiles, ucontext_t ) )
            m_ucur = cur ? &cur.m_utxt : &Fiber.sm_utxt;

        setThis( this );
        this.switchIn();
        setThis( cur );

        static if ( __traits( compiles, ucontext_t ) )
            m_ucur = null;

        // NOTE: If the fiber has terminated then the stack pointers must be
        //       reset.  This ensures that the stack for this fiber is not
        //       scanned if the fiber has terminated.  This is necessary to
        //       prevent any references lingering on the stack from delaying
        //       the collection of otherwise dead objects.  The most notable
        //       being the current object, which is referenced at the top of
        //       fiber_entryPoint.
        if ( m_state == State.TERM )
        {
            m_ctxt.tstack = m_ctxt.bstack;
        }
    }

    /// Flag to control rethrow behavior of $(D $(LREF call))
    enum Rethrow : bool { no, yes }

    /**
     * Resets this fiber so that it may be re-used, optionally with a
     * new function/delegate.  This routine should only be called for
     * fibers that have terminated, as doing otherwise could result in
     * scope-dependent functionality that is not executed.
     * Stack-based classes, for example, may not be cleaned up
     * properly if a fiber is reset before it has terminated.
     *
     * In:
     *  This fiber must be in state TERM or HOLD.
     */
    final void reset() nothrow @nogc
    in
    {
        assert( m_state == State.TERM || m_state == State.HOLD );
    }
    do
    {
        m_ctxt.tstack = m_ctxt.bstack;
        m_state = State.HOLD;
        initStack();
        m_unhandled = null;
    }

    /// ditto
    final void reset( void function() fn ) nothrow @nogc
    {
        reset();
        m_fn    = fn;
        m_call  = Call.FN;
    }

    /// ditto
    final void reset( void delegate() dg ) nothrow @nogc
    {
        reset();
        m_dg    = dg;
        m_call  = Call.DG;
    }

    ///////////////////////////////////////////////////////////////////////////
    // General Properties
    ///////////////////////////////////////////////////////////////////////////


    /// A fiber may occupy one of three states: HOLD, EXEC, and TERM.
    enum State
    {
        /** The HOLD state applies to any fiber that is suspended and ready to
        be called. */
        HOLD,
        /** The EXEC state will be set for any fiber that is currently
        executing. */
        EXEC,
        /** The TERM state is set when a fiber terminates. Once a fiber
        terminates, it must be reset before it may be called again. */
        TERM
    }


    /**
     * Gets the current state of this fiber.
     *
     * Returns:
     *  The state of this fiber as an enumerated value.
     */
    final @property State state() const @safe pure nothrow @nogc
    {
        return m_state;
    }

    /**
     * Return true if migrating a Fiber between Threads is unsafe on this
     * system.  This is due to compiler optimizations that cache thread local
     * variable addresses.  When Fiber.yield() returns on a different
     * Thread, the addresses refer to the previous Thread's variables.
     */
    static @property bool migrationUnsafe() nothrow
    {
        version (CheckFiberMigration)
            return true;
        else
            return false;
    }

    /**
     * Allow this Fiber to be resumed on a different thread for systems where
     * Fiber migration is unsafe (migrationUnsafe() is true).  Otherwise the
     * first time a Fiber is resumed on a different Thread, a ThreadException
     * is thrown.  This provides the programmer a reminder to be careful and
     * helps detect such usage in libraries being ported from other systems.
     *
     * Fiber migration on such systems can be done safely if you control all
     * the code and know that thread locals are not involved.
     *
     * For systems without this issue, allowMigration does nothing, as you are
     * always free to migrate.
     */
    final void allowMigration() nothrow
    {
        // Does nothing if checking is disabled
        version (CheckFiberMigration)
            m_allowMigration = true;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Actions on Calling Fiber
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Forces a context switch to occur away from the calling fiber.
     */
    static void yield() nothrow @nogc
    {
        Fiber   cur = getThis();
        assert( cur, "Fiber.yield() called with no active fiber" );
        assert( cur.m_state == State.EXEC );

        static if ( __traits( compiles, ucontext_t ) )
          cur.m_ucur = &cur.m_utxt;

        cur.m_state = State.HOLD;
        cur.switchOut();
        cur.m_state = State.EXEC;
    }


    /**
     * Forces a context switch to occur away from the calling fiber and then
     * throws obj in the calling fiber.
     *
     * Params:
     *  t = The object to throw.
     *
     * In:
     *  t must not be null.
     */
    static void yieldAndThrow( Throwable t ) nothrow @nogc
    in
    {
        assert( t );
    }
    do
    {
        Fiber   cur = getThis();
        assert( cur, "Fiber.yield() called with no active fiber" );
        assert( cur.m_state == State.EXEC );

        static if ( __traits( compiles, ucontext_t ) )
          cur.m_ucur = &cur.m_utxt;

        cur.m_unhandled = t;
        cur.m_state = State.HOLD;
        cur.switchOut();
        cur.m_state = State.EXEC;
    }


    ///////////////////////////////////////////////////////////////////////////
    // Fiber Accessors
    ///////////////////////////////////////////////////////////////////////////


    /**
     * Provides a reference to the calling fiber or null if no fiber is
     * currently active.
     *
     * Returns:
     *  The fiber object representing the calling fiber or null if no fiber
     *  is currently active within this thread. The result of deleting this object is undefined.
     */
    static Fiber getThis() @safe nothrow @nogc
    {
        return sm_this;
    }


    ///////////////////////////////////////////////////////////////////////////
    // Static Initialization
    ///////////////////////////////////////////////////////////////////////////


    version (Posix)
    {
        static this()
        {
            static if ( __traits( compiles, ucontext_t ) )
            {
              int status = getcontext( &sm_utxt );
              assert( status == 0 );
            }
        }
    }

private:
    //
    // Initializes a fiber object which has no associated executable function.
    //
    this() @safe pure nothrow @nogc
    {
        m_call = Call.NO;
    }


    //
    // Fiber entry point.  Invokes the function or delegate passed on
    // construction (if any).
    //
    final void run()
    {
        switch ( m_call )
        {
        case Call.FN:
            m_fn();
            break;
        case Call.DG:
            m_dg();
            break;
        default:
            break;
        }
    }


private:
    //
    // The type of routine passed on fiber construction.
    //
    enum Call
    {
        NO,
        FN,
        DG
    }


    //
    // Standard fiber data
    //
    Call                m_call;
    union
    {
        void function() m_fn;
        void delegate() m_dg;
    }
    bool                m_isRunning;
    Throwable           m_unhandled;
    State               m_state;

    // Set first time switchIn called to indicate this Fiber's Thread
    Thread              m_curThread;

    version (CheckFiberMigration)
    {
        bool m_allowMigration;
    }

    version (SjLj_Exceptions)
    {
        SjLjFuncContext* m_sjljExStackTop;
    }

private:
    ///////////////////////////////////////////////////////////////////////////
    // Stack Management
    ///////////////////////////////////////////////////////////////////////////


    //
    // Allocate a new stack for this fiber.
    //
    final void allocStack( size_t sz, size_t guardPageSize ) nothrow
    in
    {
        assert( !m_pmem && !m_ctxt );
    }
    do
    {
        // adjust alloc size to a multiple of PAGESIZE
        sz += PAGESIZE - 1;
        sz -= sz % PAGESIZE;

        // NOTE: This instance of Thread.Context is dynamic so Fiber objects
        //       can be collected by the GC so long as no user level references
        //       to the object exist.  If m_ctxt were not dynamic then its
        //       presence in the global context list would be enough to keep
        //       this object alive indefinitely.  An alternative to allocating
        //       room for this struct explicitly would be to mash it into the
        //       base of the stack being allocated below.  However, doing so
        //       requires too much special logic to be worthwhile.
        m_ctxt = new Thread.Context;

        static if ( __traits( compiles, VirtualAlloc ) )
        {
            // reserve memory for stack
            m_pmem = VirtualAlloc( null,
                                   sz + guardPageSize,
                                   MEM_RESERVE,
                                   PAGE_NOACCESS );
            if ( !m_pmem )
                onOutOfMemoryError();

            version (StackGrowsDown)
            {
                void* stack = m_pmem + guardPageSize;
                void* guard = m_pmem;
                void* pbase = stack + sz;
            }
            else
            {
                void* stack = m_pmem;
                void* guard = m_pmem + sz;
                void* pbase = stack;
            }

            // allocate reserved stack segment
            stack = VirtualAlloc( stack,
                                  sz,
                                  MEM_COMMIT,
                                  PAGE_READWRITE );
            if ( !stack )
                onOutOfMemoryError();

            if (guardPageSize)
            {
                // allocate reserved guard page
                guard = VirtualAlloc( guard,
                                      guardPageSize,
                                      MEM_COMMIT,
                                      PAGE_READWRITE | PAGE_GUARD );
                if ( !guard )
                    onOutOfMemoryError();
            }

            m_ctxt.bstack = pbase;
            m_ctxt.tstack = pbase;
            m_size = sz;
        }
        else
        {
            version (Posix) import core.sys.posix.sys.mman; // mmap
            version (FreeBSD) import core.sys.freebsd.sys.mman : MAP_ANON;
            version (NetBSD) import core.sys.netbsd.sys.mman : MAP_ANON;
            version (OpenBSD) import core.sys.openbsd.sys.mman : MAP_ANON;
            version (DragonFlyBSD) import core.sys.dragonflybsd.sys.mman : MAP_ANON;
            version (CRuntime_Glibc) import core.sys.linux.sys.mman : MAP_ANON;
            version (Darwin) import core.sys.darwin.sys.mman : MAP_ANON;
            version (CRuntime_UClibc) import core.sys.linux.sys.mman : MAP_ANON;

            static if ( __traits( compiles, mmap ) )
            {
                // Allocate more for the memory guard
                sz += guardPageSize;

                m_pmem = mmap( null,
                               sz,
                               PROT_READ | PROT_WRITE,
                               MAP_PRIVATE | MAP_ANON,
                               -1,
                               0 );
                if ( m_pmem == MAP_FAILED )
                    m_pmem = null;
            }
            else static if ( __traits( compiles, valloc ) )
            {
                m_pmem = valloc( sz );
            }
            else static if ( __traits( compiles, malloc ) )
            {
                m_pmem = malloc( sz );
            }
            else
            {
                m_pmem = null;
            }

            if ( !m_pmem )
                onOutOfMemoryError();

            version (StackGrowsDown)
            {
                m_ctxt.bstack = m_pmem + sz;
                m_ctxt.tstack = m_pmem + sz;
                void* guard = m_pmem;
            }
            else
            {
                m_ctxt.bstack = m_pmem;
                m_ctxt.tstack = m_pmem;
                void* guard = m_pmem + sz - guardPageSize;
            }
            m_size = sz;

            static if ( __traits( compiles, mmap ) )
            {
                if (guardPageSize)
                {
                    // protect end of stack
                    if ( mprotect(guard, guardPageSize, PROT_NONE) == -1 )
                        abort();
                }
            }
            else
            {
                // Supported only for mmap allocated memory - results are
                // undefined if applied to memory not obtained by mmap
            }
        }

        Thread.add( m_ctxt );
    }


    //
    // Free this fiber's stack.
    //
    final void freeStack() nothrow @nogc
    in
    {
        assert( m_pmem && m_ctxt );
    }
    do
    {
        // NOTE: m_ctxt is guaranteed to be alive because it is held in the
        //       global context list.
        Thread.slock.lock_nothrow();
        scope(exit) Thread.slock.unlock_nothrow();
        Thread.remove( m_ctxt );

        static if ( __traits( compiles, VirtualAlloc ) )
        {
            VirtualFree( m_pmem, 0, MEM_RELEASE );
        }
        else
        {
            import core.sys.posix.sys.mman; // munmap

            static if ( __traits( compiles, mmap ) )
            {
                munmap( m_pmem, m_size );
            }
            else static if ( __traits( compiles, valloc ) )
            {
                free( m_pmem );
            }
            else static if ( __traits( compiles, malloc ) )
            {
                free( m_pmem );
            }
        }
        m_pmem = null;
        m_ctxt = null;
    }


    //
    // Initialize the allocated stack.
    // Look above the definition of 'class Fiber' for some information about the implementation of this routine
    //
    final void initStack() nothrow @nogc
    in
    {
        assert( m_ctxt.tstack && m_ctxt.tstack == m_ctxt.bstack );
        assert( cast(size_t) m_ctxt.bstack % (void*).sizeof == 0 );
    }
    do
    {
        void* pstack = m_ctxt.tstack;
        scope( exit )  m_ctxt.tstack = pstack;

        void push( size_t val ) nothrow
        {
            version (StackGrowsDown)
            {
                pstack -= size_t.sizeof;
                *(cast(size_t*) pstack) = val;
            }
            else
            {
                pstack += size_t.sizeof;
                *(cast(size_t*) pstack) = val;
            }
        }

        // NOTE: On OS X the stack must be 16-byte aligned according
        // to the IA-32 call spec. For x86_64 the stack also needs to
        // be aligned to 16-byte according to SysV AMD64 ABI.
        version (AlignFiberStackTo16Byte)
        {
            version (StackGrowsDown)
            {
                pstack = cast(void*)(cast(size_t)(pstack) - (cast(size_t)(pstack) & 0x0F));
            }
            else
            {
                pstack = cast(void*)(cast(size_t)(pstack) + (cast(size_t)(pstack) & 0x0F));
            }
        }

        version (AsmX86_Windows)
        {
            version (StackGrowsDown) {} else static assert( false );

            // On Windows Server 2008 and 2008 R2, an exploit mitigation
            // technique known as SEHOP is activated by default. To avoid
            // hijacking of the exception handler chain, the presence of a
            // Windows-internal handler (ntdll.dll!FinalExceptionHandler) at
            // its end is tested by RaiseException. If it is not present, all
            // handlers are disregarded, and the program is thus aborted
            // (see http://blogs.technet.com/b/srd/archive/2009/02/02/
            // preventing-the-exploitation-of-seh-overwrites-with-sehop.aspx).
            // For new threads, this handler is installed by Windows immediately
            // after creation. To make exception handling work in fibers, we
            // have to insert it for our new stacks manually as well.
            //
            // To do this, we first determine the handler by traversing the SEH
            // chain of the current thread until its end, and then construct a
            // registration block for the last handler on the newly created
            // thread. We then continue to push all the initial register values
            // for the first context switch as for the other implementations.
            //
            // Note that this handler is never actually invoked, as we install
            // our own one on top of it in the fiber entry point function.
            // Thus, it should not have any effects on OSes not implementing
            // exception chain verification.

            alias fp_t = void function(); // Actual signature not relevant.
            static struct EXCEPTION_REGISTRATION
            {
                EXCEPTION_REGISTRATION* next; // sehChainEnd if last one.
                fp_t handler;
            }
            enum sehChainEnd = cast(EXCEPTION_REGISTRATION*) 0xFFFFFFFF;

            __gshared static fp_t finalHandler = null;
            if ( finalHandler is null )
            {
                version (LDC)
                {
                    static EXCEPTION_REGISTRATION* fs0() nothrow @naked
                    {
                        return __asm!(EXCEPTION_REGISTRATION*)("mov %fs:(0), $0", "=r");
                    }
                }
                else
                {
                    static EXCEPTION_REGISTRATION* fs0() nothrow
                    {
                        asm pure nothrow @nogc
                        {
                            naked;
                            mov EAX, FS:[0];
                            ret;
                        }
                    }
                }

                auto reg = fs0();
                while ( reg.next != sehChainEnd ) reg = reg.next;

                // Benign races are okay here, just to avoid re-lookup on every
                // fiber creation.
                finalHandler = reg.handler;
            }

            // When linking with /safeseh (supported by LDC, but not DMD)
            // the exception chain must not extend to the very top
            // of the stack, otherwise the exception chain is also considered
            // invalid. Reserving additional 4 bytes at the top of the stack will
            // keep the EXCEPTION_REGISTRATION below that limit
            size_t reserve = EXCEPTION_REGISTRATION.sizeof + 4;
            pstack -= reserve;
            *(cast(EXCEPTION_REGISTRATION*)pstack) =
                EXCEPTION_REGISTRATION( sehChainEnd, finalHandler );
            auto pChainEnd = pstack;

            push( cast(size_t) &fiber_entryPoint );                 // EIP
            push( cast(size_t) m_ctxt.bstack - reserve );           // EBP
            push( 0x00000000 );                                     // EDI
            push( 0x00000000 );                                     // ESI
            push( 0x00000000 );                                     // EBX
            push( cast(size_t) pChainEnd );                         // FS:[0]
            push( cast(size_t) m_ctxt.bstack );                     // FS:[4]
            push( cast(size_t) m_ctxt.bstack - m_size );            // FS:[8]
            push( 0x00000000 );                                     // EAX
        }
        else version (AsmX86_64_Windows)
        {
            // Using this trampoline instead of the raw fiber_entryPoint
            // ensures that during context switches, source and destination
            // stacks have the same alignment. Otherwise, the stack would need
            // to be shifted by 8 bytes for the first call, as fiber_entryPoint
            // is an actual function expecting a stack which is not aligned
            // to 16 bytes.
            version (LDC)
            {
                static void trampoline() @naked
                {
                    __asm(
                       `sub $$32, %rsp
                        call fiber_entryPoint
                        xor %rcx, %rcx
                        jmp *%rcx`,
                        "~{rsp},~{rcx}"
                    );
                }
            }
            else
            {
                static void trampoline()
                {
                    asm pure nothrow @nogc
                    {
                        naked;
                        sub RSP, 32; // Shadow space (Win64 calling convention)
                        call fiber_entryPoint;
                        xor RCX, RCX; // This should never be reached, as
                        jmp RCX;      // fiber_entryPoint must never return.
                    }
                }
            }

            push( cast(size_t) &trampoline );                       // RIP
            push( 0x00000000_00000000 );                            // RBP
            push( 0x00000000_00000000 );                            // R12
            push( 0x00000000_00000000 );                            // R13
            push( 0x00000000_00000000 );                            // R14
            push( 0x00000000_00000000 );                            // R15
            push( 0x00000000_00000000 );                            // RDI
            push( 0x00000000_00000000 );                            // RSI
            push( 0x00000000_00000000 );                            // XMM6 (high)
            push( 0x00000000_00000000 );                            // XMM6 (low)
            push( 0x00000000_00000000 );                            // XMM7 (high)
            push( 0x00000000_00000000 );                            // XMM7 (low)
            push( 0x00000000_00000000 );                            // XMM8 (high)
            push( 0x00000000_00000000 );                            // XMM8 (low)
            push( 0x00000000_00000000 );                            // XMM9 (high)
            push( 0x00000000_00000000 );                            // XMM9 (low)
            push( 0x00000000_00000000 );                            // XMM10 (high)
            push( 0x00000000_00000000 );                            // XMM10 (low)
            push( 0x00000000_00000000 );                            // XMM11 (high)
            push( 0x00000000_00000000 );                            // XMM11 (low)
            push( 0x00000000_00000000 );                            // XMM12 (high)
            push( 0x00000000_00000000 );                            // XMM12 (low)
            push( 0x00000000_00000000 );                            // XMM13 (high)
            push( 0x00000000_00000000 );                            // XMM13 (low)
            push( 0x00000000_00000000 );                            // XMM14 (high)
            push( 0x00000000_00000000 );                            // XMM14 (low)
            push( 0x00000000_00000000 );                            // XMM15 (high)
            push( 0x00000000_00000000 );                            // XMM15 (low)
            push( 0x00000000_00000000 );                            // RBX
            push( 0xFFFFFFFF_FFFFFFFF );                            // GS:[0]
            version (StackGrowsDown)
            {
                push( cast(size_t) m_ctxt.bstack );                 // GS:[8]
                push( cast(size_t) m_ctxt.bstack - m_size );        // GS:[16]
            }
            else
            {
                push( cast(size_t) m_ctxt.bstack );                 // GS:[8]
                push( cast(size_t) m_ctxt.bstack + m_size );        // GS:[16]
            }
        }
        else version (AsmX86_Posix)
        {
            push( 0x00000000 );                                     // Return address of fiber_entryPoint call
            push( cast(size_t) &fiber_entryPoint );                 // EIP
            push( cast(size_t) m_ctxt.bstack );                     // EBP
            push( 0x00000000 );                                     // EDI
            push( 0x00000000 );                                     // ESI
            push( 0x00000000 );                                     // EBX
            push( 0x00000000 );                                     // EAX
        }
        else version (AsmX86_64_Posix)
        {
            push( 0x00000000_00000000 );                            // Return address of fiber_entryPoint call
            push( cast(size_t) &fiber_entryPoint );                 // RIP
            push( cast(size_t) m_ctxt.bstack );                     // RBP
            push( 0x00000000_00000000 );                            // RBX
            push( 0x00000000_00000000 );                            // R12
            push( 0x00000000_00000000 );                            // R13
            push( 0x00000000_00000000 );                            // R14
            push( 0x00000000_00000000 );                            // R15
        }
        else version (AsmPPC_Posix)
        {
            version (StackGrowsDown)
            {
                pstack -= int.sizeof * 5;
            }
            else
            {
                pstack += int.sizeof * 5;
            }

            push( cast(size_t) &fiber_entryPoint );     // link register
            push( 0x00000000 );                         // control register
            push( 0x00000000 );                         // old stack pointer

            // GPR values
            version (StackGrowsDown)
            {
                pstack -= int.sizeof * 20;
            }
            else
            {
                pstack += int.sizeof * 20;
            }

            assert( (cast(size_t) pstack & 0x0f) == 0 );
        }
        else version (AsmPPC64_Posix)
        {
            version (StackGrowsDown) {}
            else static assert(0);

            /*
             * The stack frame uses the standard layout except for floating
             * point and vector registers.
             *
             * ELFv2:
             * +------------------------+
             * | TOC Pointer Doubleword | SP+24
             * +------------------------+
             * | LR Save Doubleword     | SP+16
             * +------------------------+
             * | Reserved               | SP+12
             * +------------------------+
             * | CR Save Word           | SP+8
             * +------------------------+
             * | Back Chain             | SP+176 <-- Previous function
             * +------------------------+
             * | GPR Save Area (14-31)  | SP+32
             * +------------------------+
             * | TOC Pointer Doubleword | SP+24
             * +------------------------+
             * | LR Save Doubleword     | SP+16
             * +------------------------+
             * | Reserved               | SP+12
             * +------------------------+
             * | CR Save Word           | SP+8
             * +------------------------+
             * | Back Chain             | SP+0   <-- Stored stack pointer
             * +------------------------+
             * | VR Save Area (20-31)   | SP-16
             * +------------------------+
             * | FPR Save Area (14-31)  | SP-200
             * +------------------------+
             *
             * ELFv1:
             * +------------------------+
             * | Parameter Save Area    | SP+48
             * +------------------------+
             * | TOC Pointer Doubleword | SP+40
             * +------------------------+
             * | Link editor doubleword | SP+32
             * +------------------------+
             * | Compiler Doubleword    | SP+24
             * +------------------------+
             * | LR Save Doubleword     | SP+16
             * +------------------------+
             * | Reserved               | SP+12
             * +------------------------+
             * | CR Save Word           | SP+8
             * +------------------------+
             * | Back Chain             | SP+256 <-- Previous function
             * +------------------------+
             * | GPR Save Area (14-31)  | SP+112
             * +------------------------+
             * | Parameter Save Area    | SP+48
             * +------------------------+
             * | TOC Pointer Doubleword | SP+40
             * +------------------------+
             * | Link editor doubleword | SP+32
             * +------------------------+
             * | Compiler Doubleword    | SP+24
             * +------------------------+
             * | LR Save Doubleword     | SP+16
             * +------------------------+
             * | Reserved               | SP+12
             * +------------------------+
             * | CR Save Word           | SP+8
             * +------------------------+
             * | Back Chain             | SP+0   <-- Stored stack pointer
             * +------------------------+
             * | VR Save Area (20-31)   | SP-16
             * +------------------------+
             * | FPR Save Area (14-31)  | SP-200
             * +------------------------+
             */
            assert( (cast(size_t) pstack & 0x0f) == 0 );
            version (ELFv1)
            {
                pstack -= size_t.sizeof * 8;                // Parameter Save Area
                push( 0x00000000_00000000 );                // TOC Pointer Doubleword
                push( 0x00000000_00000000 );                // Link editor doubleword
                push( 0x00000000_00000000 );                // Compiler Doubleword
                push( cast(size_t) &fiber_entryPoint );     // LR Save Doubleword
                push( 0x00000000_00000000 );                // CR Save Word
                push( 0x00000000_00000000 );                // Back Chain
                size_t backchain = cast(size_t) pstack;     // Save back chain
                pstack -= size_t.sizeof * 18;               // GPR Save Area
                pstack -= size_t.sizeof * 8;                // Parameter Save Area
                push( 0x00000000_00000000 );                // TOC Pointer Doubleword
                push( 0x00000000_00000000 );                // Link editor doubleword
                push( 0x00000000_00000000 );                // Compiler Doubleword
                push( 0x00000000_00000000 );                // LR Save Doubleword
                push( 0x00000000_00000000 );                // CR Save Word
                push( backchain );                          // Back Chain
            }
            else
            {
                push( 0x00000000_00000000 );                // TOC Pointer Doubleword
                push( cast(size_t) &fiber_entryPoint );     // LR Save Doubleword
                push( 0x00000000_00000000 );                // CR Save Word
                push( 0x00000000_00000000 );                // Back Chain
                size_t backchain = cast(size_t) pstack;     // Save back chain
                pstack -= size_t.sizeof * 18;               // GPR Save Area
                push( 0x00000000_00000000 );                // TOC Pointer Doubleword
                push( 0x00000000_00000000 );                // LR Save Doubleword
                push( 0x00000000_00000000 );                // CR Save Word
                push( backchain );                          // Back Chain
            }
            assert( (cast(size_t) pstack & 0x0f) == 0 );
        }
        else version (AsmMIPS_O32_Posix)
        {
            version (StackGrowsDown) {}
            else static assert(0);

            /* We keep the FP registers and the return address below
             * the stack pointer, so they don't get scanned by the
             * GC. The last frame before swapping the stack pointer is
             * organized like the following.
             *
             *     |-----------|<= frame pointer
             *     |    $gp    |
             *     |   $s0-8   |
             *     |-----------|<= stack pointer
             *     |    $ra    |
             *     |  align(8) |
             *     |  $f20-30  |
             *     |-----------|
             *
             */
            enum SZ_GP = 10 * size_t.sizeof; // $gp + $s0-8
            enum SZ_RA = size_t.sizeof;      // $ra
            version (MIPS_HardFloat)
            {
                enum SZ_FP = 6 * 8;          // $f20-30
                enum ALIGN = -(SZ_FP + SZ_RA) & (8 - 1);
            }
            else
            {
                enum SZ_FP = 0;
                enum ALIGN = 0;
            }

            enum BELOW = SZ_FP + ALIGN + SZ_RA;
            enum ABOVE = SZ_GP;
            enum SZ = BELOW + ABOVE;

            (cast(ubyte*)pstack - SZ)[0 .. SZ] = 0;
            pstack -= ABOVE;
            *cast(size_t*)(pstack - SZ_RA) = cast(size_t)&fiber_entryPoint;
        }
        else version (AsmAArch64_Posix)
        {
            // Like others, FP registers and return address (lr) are kept
            // below the saved stack top (tstack) to hide from GC scanning.
            // fiber_switchContext expects newp sp to look like this:
            //   19: x19
            //   ...
            //    9: x29 (fp)  <-- newp tstack
            //    8: x30 (lr)  [&fiber_entryPoint]
            //    7: d8
            //   ...
            //    0: d15

            version (StackGrowsDown) {}
            else
                static assert(false, "Only full descending stacks supported on AArch64");

            // Only need to set return address (lr).  Everything else is fine
            // zero initialized.
            pstack -= size_t.sizeof * 11;    // skip past x19-x29
            push(cast(size_t) &fiber_trampoline); // see threadasm.S for docs
            pstack += size_t.sizeof;         // adjust sp (newp) above lr
        }
        else version (AsmARM_Posix)
        {
            /* We keep the FP registers and the return address below
             * the stack pointer, so they don't get scanned by the
             * GC. The last frame before swapping the stack pointer is
             * organized like the following.
             *
             *   |  |-----------|<= 'frame starts here'
             *   |  |     fp    | (the actual frame pointer, r11 isn't
             *   |  |   r10-r4  |  updated and still points to the previous frame)
             *   |  |-----------|<= stack pointer
             *   |  |     lr    |
             *   |  | 4byte pad |
             *   |  |   d15-d8  |(if FP supported)
             *   |  |-----------|
             *   Y
             *   stack grows down: The pointer value here is smaller than some lines above
             */
            // frame pointer can be zero, r10-r4 also zero initialized
            version (StackGrowsDown)
                pstack -= int.sizeof * 8;
            else
                static assert(false, "Only full descending stacks supported on ARM");

            // link register
            push( cast(size_t) &fiber_entryPoint );
            /*
             * We do not push padding and d15-d8 as those are zero initialized anyway
             * Position the stack pointer above the lr register
             */
            pstack += int.sizeof * 1;
        }
        else static if ( __traits( compiles, ucontext_t ) )
        {
            getcontext( &m_utxt );
            m_utxt.uc_stack.ss_sp   = m_pmem;
            m_utxt.uc_stack.ss_size = m_size;
            makecontext( &m_utxt, &fiber_entryPoint, 0 );
            // NOTE: If ucontext is being used then the top of the stack will
            //       be a pointer to the ucontext_t struct for that fiber.
            push( cast(size_t) &m_utxt );
        }
        else
            static assert(0, "Not implemented");
    }


    Thread.Context* m_ctxt;
    size_t          m_size;
    void*           m_pmem;

    static if ( __traits( compiles, ucontext_t ) )
    {
        // NOTE: The static ucontext instance is used to represent the context
        //       of the executing thread.
        static ucontext_t       sm_utxt = void;
        ucontext_t              m_utxt  = void;
        ucontext_t*             m_ucur  = null;
    }


private:
    ///////////////////////////////////////////////////////////////////////////
    // Storage of Active Fiber
    ///////////////////////////////////////////////////////////////////////////


    //
    // Sets a thread-local reference to the current fiber object.
    //
    static void setThis( Fiber f ) nothrow @nogc
    {
        sm_this = f;
    }

    static Fiber sm_this;


private:
    ///////////////////////////////////////////////////////////////////////////
    // Context Switching
    ///////////////////////////////////////////////////////////////////////////


    //
    // Switches into the stack held by this fiber.
    //
    final void switchIn() nothrow @nogc
    {
        Thread  tobj = Thread.getThis();
        void**  oldp = &tobj.m_curr.tstack;
        void*   newp = m_ctxt.tstack;

        version (CheckFiberMigration)
        {
            if (m_curThread is null || m_allowMigration)
                m_curThread = tobj;
            else if (tobj !is m_curThread)
            {
                // allocate and construct a ThreadException manually for @nogc
                import core.stdc.stdlib : malloc;
                enum threadExceptionSize = __traits(classInstanceSize, ThreadException);
                if (void* p = malloc(threadExceptionSize))
                {
                    p[0 .. threadExceptionSize] = typeid(ThreadException).initializer[];
                    auto e = cast(ThreadException) p;
                    e.__ctor(
                        "Migrating Fibers between Threads on this platform may lead " ~
                        "to incorrect thread local variable access.  To allow " ~
                        "migration anyway, call Fiber.allowMigration()");
                    m_unhandled = e;
                    // the exception will leak...
                }
                return;
            }
        }
        else
        {
            m_curThread = tobj;
        }

        version (SjLj_Exceptions)
            SjLjFuncContext* oldsjlj = swapSjLjStackTop(m_sjljExStackTop);

        // NOTE: The order of operations here is very important.  The current
        //       stack top must be stored before m_lock is set, and pushContext
        //       must not be called until after m_lock is set.  This process
        //       is intended to prevent a race condition with the suspend
        //       mechanism used for garbage collection.  If it is not followed,
        //       a badly timed collection could cause the GC to scan from the
        //       bottom of one stack to the top of another, or to miss scanning
        //       a stack that still contains valid data.  The old stack pointer
        //       oldp will be set again before the context switch to guarantee
        //       that it points to exactly the correct stack location so the
        //       successive pop operations will succeed.
        *oldp = getStackTop();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, true);
        tobj.pushContext( m_ctxt );

        version (SupportSanitizers)
        {
            version (StackGrowsDown)
                auto new_bottom = m_ctxt.bstack - m_size;
            else
                auto new_bottom = m_ctxt.bstack;

            informSanitizerOfStartSwitchFiber(&__fake_stack, new_bottom, m_size);
        }

        fiber_switchContext( oldp, newp );

        version (SupportSanitizers)
        {
            informSanitizerOfFinishSwitchFiber((m_state == State.TERM) ? null : __fake_stack,
                                               &__from_stack_bottom, &__from_stack_size);
        }

        // NOTE: As above, these operations must be performed in a strict order
        //       to prevent Bad Things from happening.
        tobj.popContext();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, false);
        tobj.m_curr.tstack = tobj.m_curr.bstack;

        version (SjLj_Exceptions)
            m_sjljExStackTop = swapSjLjStackTop(oldsjlj);
    }


    //
    // Switches out of the current stack and into the enclosing stack.
    //
    final void switchOut() nothrow @nogc
    {
        Thread  tobj = m_curThread;
        void**  oldp = &m_ctxt.tstack;
        void*   newp = tobj.m_curr.within.tstack;

        // NOTE: The order of operations here is very important.  The current
        //       stack top must be stored before m_lock is set, and pushContext
        //       must not be called until after m_lock is set.  This process
        //       is intended to prevent a race condition with the suspend
        //       mechanism used for garbage collection.  If it is not followed,
        //       a badly timed collection could cause the GC to scan from the
        //       bottom of one stack to the top of another, or to miss scanning
        //       a stack that still contains valid data.  The old stack pointer
        //       oldp will be set again before the context switch to guarantee
        //       that it points to exactly the correct stack location so the
        //       successive pop operations will succeed.
        *oldp = getStackTop();
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, true);

        version (SupportSanitizers)
        {
            informSanitizerOfStartSwitchFiber(&__fake_stack, __from_stack_bottom, __from_stack_size);
        }

        fiber_switchContext( oldp, newp );

        version (SupportSanitizers)
        {
            informSanitizerOfFinishSwitchFiber(__fake_stack, &__from_stack_bottom, &__from_stack_size);
        }

        // NOTE: As above, these operations must be performed in a strict order
        //       to prevent Bad Things from happening.
        // NOTE: If use of this fiber is multiplexed across threads, the thread
        //       executing here may be different from the one above, so get the
        //       current thread handle before unlocking, etc.
        tobj = m_curThread;
        atomicStore!(MemoryOrder.raw)(*cast(shared)&tobj.m_lock, false);
        tobj.m_curr.tstack = tobj.m_curr.bstack;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Address Sanitizer support
    ///////////////////////////////////////////////////////////////////////////
    version (SupportSanitizers)
    {
    private:
        void* __fake_stack;
        const(void)* __from_stack_bottom;
        size_t __from_stack_size;
    }
}

///
unittest {
    int counter;

    class DerivedFiber : Fiber
    {
        this()
        {
            super( &run );
        }

    private :
        void run()
        {
            counter += 2;
        }
    }

    void fiberFunc()
    {
        counter += 4;
        Fiber.yield();
        counter += 8;
    }

    // create instances of each type
    Fiber derived = new DerivedFiber();
    Fiber composed = new Fiber( &fiberFunc );

    assert( counter == 0 );

    derived.call();
    assert( counter == 2, "Derived fiber increment." );

    composed.call();
    assert( counter == 6, "First composed fiber increment." );

    counter += 16;
    assert( counter == 22, "Calling context increment." );

    composed.call();
    assert( counter == 30, "Second composed fiber increment." );

    // since each fiber has run to completion, each should have state TERM
    assert( derived.state == Fiber.State.TERM );
    assert( composed.state == Fiber.State.TERM );
}

version (unittest)
{
    class TestFiber : Fiber
    {
        this()
        {
            super(&run);
        }

        void run()
        {
            foreach (i; 0 .. 1000)
            {
                sum += i;
                Fiber.yield();
            }
        }

        enum expSum = 1000 * 999 / 2;
        size_t sum;
    }

    void runTen()
    {
        TestFiber[10] fibs;
        foreach (ref fib; fibs)
            fib = new TestFiber();

        bool cont;
        do {
            cont = false;
            foreach (fib; fibs) {
                if (fib.state == Fiber.State.HOLD)
                {
                    fib.call();
                    cont |= fib.state != Fiber.State.TERM;
                }
            }
        } while (cont);

        foreach (fib; fibs)
        {
            assert(fib.sum == TestFiber.expSum);
        }
    }
}


// Single thread running separate fibers
unittest
{
    runTen();
}


// Multiple threads running separate fibers
unittest
{
    auto group = new ThreadGroup();
    foreach (_; 0 .. 4)
    {
        group.create(&runTen);
    }
    group.joinAll();
}

// Try to detect if version CheckFiberMigration should be set, or if it is
// set, make sure it behaves properly.  The issue is that thread local addr
// may be cached by the compiler and that doesn't play well when Fibers
// migrate between Threads.  This may only happen when optimization
// enabled.  For that reason, should run this unittest with various
// optimization levels.
//
// https://github.com/ldc-developers/ldc/issues/666
unittest
{
    static int tls;

    static yield_noinline()
    {
        import ldc.intrinsics;
        pragma(LDC_never_inline);
        Fiber.yield();
    }

    auto f = new Fiber(
    {
        ++tls;                               // happens on main thread
        yield_noinline();
        ++tls;                               // happens on other thread,
        // 1 if tls addr uncached, 2 if addr was cached
    });

    auto t = new Thread(
    {
        assert(tls == 0);

        version (CheckFiberMigration)
        {
            try
            {
                f.call();
                assert(false, "Should get ThreadException when Fiber migrated");
            }
            catch (ThreadException ex)
            {
            }

            f.allowMigration();
        }

        f.call();
        // tls may be 0 (wrong) or 1 (good) depending on thread local handling
        // by compiler
    });

    assert(tls == 0);
    f.call();
    assert(tls == 1);

    t.start();
    t.join();

    version (CheckFiberMigration)
    {
        assert(Fiber.migrationUnsafe);
    }
    else version (FiberMigrationTest)
    {
        assert(!Fiber.migrationUnsafe);
        // If thread local addr not cached (correct behavior), then tls should
        // still be 1.
        assert(tls != 2,
               "Not safe to migrate Fibers between Threads on your system. " ~
               "Consider setting version CheckFiberMigration for this system " ~
               "in thread.d");
        // verify un-cached correct case
        assert(tls == 1);
    }
    else
    {
        if (tls == 2)
        {
            import core.stdc.stdio : puts;
            puts("Not safe to migrate Fibers between Threads on your system. " ~
                 "Consider setting version CheckFiberMigration for this system " ~
                 "in thread.d");
        }
    }
}

// Multiple threads running shared fibers
unittest
{
    shared bool[10] locks;
    TestFiber[10] fibs;

    void runShared()
    {
        bool cont;
        do {
            cont = false;
            foreach (idx; 0 .. 10)
            {
                if (cas(&locks[idx], false, true))
                {
                    if (fibs[idx].state == Fiber.State.HOLD)
                    {
                        fibs[idx].call();
                        cont |= fibs[idx].state != Fiber.State.TERM;
                    }
                    locks[idx] = false;
                }
                else
                {
                    cont = true;
                }
            }
        } while (cont);
    }

    foreach (ref fib; fibs)
    {
        fib = new TestFiber();
        fib.allowMigration();
    }

    auto group = new ThreadGroup();
    foreach (_; 0 .. 4)
    {
        group.create(&runShared);
    }
    group.joinAll();

    foreach (fib; fibs)
    {
        assert(fib.sum == TestFiber.expSum);
    }
}


// Test exception handling inside fibers.
unittest
{
    enum MSG = "Test message.";
    string caughtMsg;
    (new Fiber({
        try
        {
            throw new Exception(MSG);
        }
        catch (Exception e)
        {
            caughtMsg = e.msg;
        }
    })).call();
    assert(caughtMsg == MSG);
}


unittest
{
    int x = 0;

    (new Fiber({
        x++;
    })).call();
    assert( x == 1 );
}

nothrow unittest
{
    new Fiber({}).call!(Fiber.Rethrow.no)();
}

unittest
{
    new Fiber({}).call(Fiber.Rethrow.yes);
    new Fiber({}).call(Fiber.Rethrow.no);
}

unittest
{
    enum MSG = "Test message.";

    try
    {
        (new Fiber({
            throw new Exception( MSG );
        })).call();
        assert( false, "Expected rethrown exception." );
    }
    catch ( Throwable t )
    {
        assert( t.msg == MSG );
    }
}

// Test exception chaining when switching contexts in finally blocks.
unittest
{
    static void throwAndYield(string msg) {
      try {
        throw new Exception(msg);
      } finally {
        Fiber.yield();
      }
    }

    static void fiber(string name) {
      try {
        try {
          throwAndYield(name ~ ".1");
        } finally {
          throwAndYield(name ~ ".2");
        }
      } catch (Exception e) {
        assert(e.msg == name ~ ".1");
        assert(e.next);
        assert(e.next.msg == name ~ ".2");
        assert(!e.next.next);
      }
    }

    auto first = new Fiber(() => fiber("first"));
    auto second = new Fiber(() => fiber("second"));
    first.call();
    second.call();
    first.call();
    second.call();
    first.call();
    second.call();
    assert(first.state == Fiber.State.TERM);
    assert(second.state == Fiber.State.TERM);
}

// Test Fiber resetting
unittest
{
    static string method;

    static void foo()
    {
        method = "foo";
    }

    void bar()
    {
        method = "bar";
    }

    static void expect(Fiber fib, string s)
    {
        assert(fib.state == Fiber.State.HOLD);
        fib.call();
        assert(fib.state == Fiber.State.TERM);
        assert(method == s); method = null;
    }
    auto fib = new Fiber(&foo);
    expect(fib, "foo");

    fib.reset();
    expect(fib, "foo");

    fib.reset(&foo);
    expect(fib, "foo");

    fib.reset(&bar);
    expect(fib, "bar");

    fib.reset(function void(){method = "function";});
    expect(fib, "function");

    fib.reset(delegate void(){method = "delegate";});
    expect(fib, "delegate");
}

// Test unsafe reset in hold state
unittest
{
    auto fib = new Fiber(function {ubyte[2048] buf = void; Fiber.yield();}, 4096);
    foreach (_; 0 .. 10)
    {
        fib.call();
        assert(fib.state == Fiber.State.HOLD);
        fib.reset();
    }
}

// stress testing GC stack scanning
unittest
{
    import core.memory;
    import core.time : dur;

    static void unreferencedThreadObject()
    {
        static void sleep() { Thread.sleep(dur!"msecs"(100)); }
        auto thread = new Thread(&sleep).start();
    }
    unreferencedThreadObject();
    GC.collect();

    static class Foo
    {
        this(int value)
        {
            _value = value;
        }

        int bar()
        {
            return _value;
        }

        int _value;
    }

    static void collect()
    {
        auto foo = new Foo(2);
        assert(foo.bar() == 2);
        GC.collect();
        Fiber.yield();
        GC.collect();
        assert(foo.bar() == 2);
    }

    auto fiber = new Fiber(&collect);

    fiber.call();
    GC.collect();
    fiber.call();

    // thread reference
    auto foo = new Foo(2);

    void collect2()
    {
        assert(foo.bar() == 2);
        GC.collect();
        Fiber.yield();
        GC.collect();
        assert(foo.bar() == 2);
    }

    fiber = new Fiber(&collect2);

    fiber.call();
    GC.collect();
    fiber.call();

    static void recurse(size_t cnt)
    {
        --cnt;
        Fiber.yield();
        if (cnt)
        {
            auto fib = new Fiber(() { recurse(cnt); });
            fib.call();
            GC.collect();
            fib.call();
        }
    }
    fiber = new Fiber(() { recurse(20); });
    fiber.call();
}


version (AsmX86_64_Windows)
{
    // Test Windows x64 calling convention
    unittest
    {
        void testNonvolatileRegister(alias REG)()
        {
            auto zeroRegister = new Fiber(() {
                mixin("asm pure nothrow @nogc { naked; xor "~REG~", "~REG~"; ret; }");
            });
            long after;

            mixin("asm pure nothrow @nogc { mov "~REG~", 0xFFFFFFFFFFFFFFFF; }");
            zeroRegister.call();
            mixin("asm pure nothrow @nogc { mov after, "~REG~"; }");

            assert(after == -1);
        }

        void testNonvolatileRegisterSSE(alias REG)()
        {
            auto zeroRegister = new Fiber(() {
                mixin("asm pure nothrow @nogc { naked; xorpd "~REG~", "~REG~"; ret; }");
            });
            long[2] before = [0xFFFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF], after;

            mixin("asm pure nothrow @nogc { movdqu "~REG~", before; }");
            zeroRegister.call();
            mixin("asm pure nothrow @nogc { movdqu after, "~REG~"; }");

            assert(before == after);
        }

        testNonvolatileRegister!("R12")();
        testNonvolatileRegister!("R13")();
        testNonvolatileRegister!("R14")();
        testNonvolatileRegister!("R15")();
      version (LDC)
      {
        // FIXME: fails with `-O` (unless in separate object file)
      }
      else
      {
        testNonvolatileRegister!("RDI")();
        testNonvolatileRegister!("RSI")();
        testNonvolatileRegister!("RBX")();
      }

        testNonvolatileRegisterSSE!("XMM6")();
        testNonvolatileRegisterSSE!("XMM7")();
        testNonvolatileRegisterSSE!("XMM8")();
        testNonvolatileRegisterSSE!("XMM9")();
        testNonvolatileRegisterSSE!("XMM10")();
        testNonvolatileRegisterSSE!("XMM11")();
        testNonvolatileRegisterSSE!("XMM12")();
        testNonvolatileRegisterSSE!("XMM13")();
        testNonvolatileRegisterSSE!("XMM14")();
        testNonvolatileRegisterSSE!("XMM15")();
    }
}


version (D_InlineAsm_X86_64)
{
    unittest
    {
        void testStackAlignment()
        {
            void* pRSP;
            asm pure nothrow @nogc
            {
                mov pRSP, RSP;
            }
            assert((cast(size_t)pRSP & 0xF) == 0);
        }

        auto fib = new Fiber(&testStackAlignment);
        fib.call();
    }
}
