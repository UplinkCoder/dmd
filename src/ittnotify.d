/* <copyright>
  This file is provided under a dual BSD/GPLv2 license.  When using or
  redistributing this file, you may do so under either license.

  GPL LICENSE SUMMARY

  Copyright (c) 2005-2014 Intel Corporation. All rights reserved.

  This program is free software; you can redistribute it and/or modify
  it under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
  The full GNU General Public License is included in this distribution
  in the file called LICENSE.GPL.

  Contact Information:
  http://software.intel.com/en-us/articles/intel-vtune-amplifier-xe/

  BSD LICENSE

  Copyright (c) 2005-2014 Intel Corporation. All rights reserved.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.
    * Neither the name of Intel Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
</copyright> */

extern (C):

/**
@file
@brief Public User API functions and types
@mainpage

The ITT API is used to annotate a user's program with additional information
that can be used by correctness and performance tools. The user inserts
calls in their program. Those calls generate information that is collected
at runtime, and used by Intel(R) Threading Tools.

@section API Concepts
The following general concepts are used throughout the API.

@subsection Unicode Support
Many API functions take character string arguments. On Windows, there
are two versions of each such function. The function name is suffixed
by W if Unicode support is enabled, and by A otherwise. Any API function
that takes a character string argument adheres to this convention.

@subsection Conditional Compilation
Many users prefer having an option to modify ITT API code when linking it
inside their runtimes. ITT API header file provides a mechanism to replace
ITT API function names inside your code with empty strings. To do this,
define the macros INTEL_NO_ITTNOTIFY_API during compilation and remove the
static library from the linker script.

@subsection Domains
[see domains]
Domains provide a way to separate notification for different modules or
libraries in a program. Domains are specified by dotted character strings,
e.g. TBB.Internal.Control.

A mechanism (to be specified) is provided to enable and disable
domains. By default, all domains are enabled.
@subsection Named Entities and Instances
Named entities (frames, regions, tasks, and markers) communicate
information about the program to the analysis tools. A named entity often
refers to a section of program code, or to some set of logical concepts
that the programmer wants to group together.

Named entities relate to the programmer's static view of the program. When
the program actually executes, many instances of a given named entity
may be created.

The API annotations denote instances of named entities. The actual
named entities are displayed using the analysis tools. In other words,
the named entities come into existence when instances are created.

Instances of named entities may have instance identifiers (IDs). Some
API calls use instance identifiers to create relationships between
different instances of named entities. Other API calls associate data
with instances of named entities.

Some named entities must always have instance IDs. In particular, regions
and frames always have IDs. Task and markers need IDs only if the ID is
needed in another API call (such as adding a relation or metadata).

The lifetime of instance IDs is distinct from the lifetime of
instances. This allows various relationships to be specified separate
from the actual execution of instances. This flexibility comes at the
expense of extra API calls.

The same ID may not be reused for different instances, unless a previous
[ref] __itt_id_destroy call for that ID has been issued.
*/

/** @cond exclude_from_documentation */

enum ITT_OS_WIN = 1;
/* ITT_OS_WIN */

enum ITT_OS_LINUX = 2;
/* ITT_OS_LINUX */

enum ITT_OS_MAC = 3;
/* ITT_OS_MAC */

enum ITT_OS_FREEBSD = 4;
/* ITT_OS_FREEBSD */

enum ITT_OS = ITT_OS_LINUX;

/* ITT_OS */

enum ITT_PLATFORM_WIN = 1;
/* ITT_PLATFORM_WIN */

enum ITT_PLATFORM_POSIX = 2;
/* ITT_PLATFORM_POSIX */

enum ITT_PLATFORM_MAC = 3;
/* ITT_PLATFORM_MAC */

enum ITT_PLATFORM_FREEBSD = 4;
/* ITT_PLATFORM_FREEBSD */

enum ITT_PLATFORM = ITT_PLATFORM_POSIX;

/* ITT_PLATFORM */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* UNICODE || _UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* _M_IX86 || __i386__ */ /* actual only on x86 platform */
/* _M_IX86 || __i386__ */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* ITTAPI_CDECL */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* _M_IX86 || __i386__ */ /* supported only on x86 platform */
/* _M_IX86 || __i386__ */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* STDCALL */

/* use __forceinline (VC++ specific) */

/* nothing */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/*
 * Generally, functions are not inlined unless optimization is specified.
 * For functions declared inline, this attribute inlines the function even
 * if no optimization level was specified.
 */

/* __STRICT_ANSI__ */
/* __STRICT_ANSI__ */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/** @endcond */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* INTEL_ITTNOTIFY_ENABLE_LEGACY */

/** @cond exclude_from_documentation */
/* Helper macro for joining tokens */
extern (D) string ITT_JOIN_AUX(T0, T1)(auto ref T0 p, auto ref T1 n)
{
    import std.conv : to;

    return to!string(p) ~ to!string(n);
}

alias ITT_JOIN = ITT_JOIN_AUX;

enum ITT_MAJOR = 3;
enum ITT_MINOR = 0;

/* Standard versioning of a token with major and minor version numbers */
extern (D) auto ITT_VERSIONIZE(T)(auto ref T x)
{
    return ITT_JOIN(x, ITT_JOIN(_, ITT_JOIN(ITT_MAJOR, ITT_JOIN(_, ITT_MINOR))));
}

extern (D) auto ITTNOTIFY_NAME_AUX(T)(auto ref T n)
{
    return ITT_JOIN(INTEL_ITTNOTIFY_PREFIX, n);
}

extern (D) auto ITTNOTIFY_NAME(T)(auto ref T n)
{
    return ITT_VERSIONIZE(ITTNOTIFY_NAME_AUX(ITT_JOIN(n, INTEL_ITTNOTIFY_POSTFIX)));
}

extern (D) auto ITTNOTIFY_VOID(T)(auto ref T n)
{
    return (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n);
}

extern (D) auto ITTNOTIFY_DATA(T)(auto ref T n)
{
    return (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n);
}

extern (D) auto ITTNOTIFY_VOID_D0(T0, T1)(auto ref T0 n, auto ref T1 d)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d);
}

extern (D) auto ITTNOTIFY_VOID_D1(T0, T1, T2)(auto ref T0 n, auto ref T1 d, auto ref T2 x)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d, x);
}

extern (D) auto ITTNOTIFY_VOID_D2(T0, T1, T2, T3)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d, x, y);
}

extern (D) auto ITTNOTIFY_VOID_D3(T0, T1, T2, T3, T4)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d, x, y, z);
}

extern (D) auto ITTNOTIFY_VOID_D4(T0, T1, T2, T3, T4, T5)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z, auto ref T5 a)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d, x, y, z, a);
}

extern (D) auto ITTNOTIFY_VOID_D5(T0, T1, T2, T3, T4, T5, T6)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z, auto ref T5 a, auto ref T6 b)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d, x, y, z, a, b);
}

extern (D) auto ITTNOTIFY_VOID_D6(T0, T1, T2, T3, T4, T5, T6, T7)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z, auto ref T5 a, auto ref T6 b, auto ref T7 c)
{
    return (!d.flags) ? cast(void) 0 : (!ITTNOTIFY_NAME(n)) ? cast(void) 0 : ITTNOTIFY_NAME(n)(d, x, y, z, a, b, c);
}

extern (D) auto ITTNOTIFY_DATA_D0(T0, T1)(auto ref T0 n, auto ref T1 d)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d);
}

extern (D) auto ITTNOTIFY_DATA_D1(T0, T1, T2)(auto ref T0 n, auto ref T1 d, auto ref T2 x)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d, x);
}

extern (D) auto ITTNOTIFY_DATA_D2(T0, T1, T2, T3)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d, x, y);
}

extern (D) auto ITTNOTIFY_DATA_D3(T0, T1, T2, T3, T4)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d, x, y, z);
}

extern (D) auto ITTNOTIFY_DATA_D4(T0, T1, T2, T3, T4, T5)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z, auto ref T5 a)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d, x, y, z, a);
}

extern (D) auto ITTNOTIFY_DATA_D5(T0, T1, T2, T3, T4, T5, T6)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z, auto ref T5 a, auto ref T6 b)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d, x, y, z, a, b);
}

extern (D) auto ITTNOTIFY_DATA_D6(T0, T1, T2, T3, T4, T5, T6, T7)(auto ref T0 n, auto ref T1 d, auto ref T2 x, auto ref T3 y, auto ref T4 z, auto ref T5 a, auto ref T6 b, auto ref T7 c)
{
    return (!d.flags) ? 0 : (!ITTNOTIFY_NAME(n)) ? 0 : ITTNOTIFY_NAME(n)(d, x, y, z, a, b, c);
}

/** @endcond */

/* __cplusplus */

/** @cond exclude_from_gpa_documentation */
/**
 * @defgroup public Public API
 * @{
 * @}
 */

/**
 * @defgroup control Collection Control
 * @ingroup public
 * General behavior: application continues to run, but no profiling information is being collected
 *
 * Pausing occurs not only for the current thread but for all process as well as spawned processes
 * - Intel(R) Parallel Inspector and Intel(R) Inspector XE:
 *   - Does not analyze or report errors that involve memory access.
 *   - Other errors are reported as usual. Pausing data collection in
 *     Intel(R) Parallel Inspector and Intel(R) Inspector XE
 *     only pauses tracing and analyzing memory access.
 *     It does not pause tracing or analyzing threading APIs.
 *   .
 * - Intel(R) Parallel Amplifier and Intel(R) VTune(TM) Amplifier XE:
 *   - Does continue to record when new threads are started.
 *   .
 * - Other effects:
 *   - Possible reduction of runtime overhead.
 *   .
 * @{
 */
/** @brief Pause collection */
void __itt_pause ();
/** @brief Resume collection */
void __itt_resume ();
/** @brief Detach collection */
void __itt_detach ();

/** @cond exclude_from_documentation */

alias __itt_pause_ptr__3_0_t = void function ();
extern __gshared __itt_pause_ptr__3_0_t __itt_pause_ptr__3_0;
alias __itt_resume_ptr__3_0_t = void function ();
extern __gshared __itt_resume_ptr__3_0_t __itt_resume_ptr__3_0;
alias __itt_detach_ptr__3_0_t = void function ();
extern __gshared __itt_detach_ptr__3_0_t __itt_detach_ptr__3_0;
/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} control group */
/** @endcond */

/**
 * @defgroup threads Threads
 * @ingroup public
 * Give names to threads
 * @{
 */
/**
 * @brief Sets thread name of calling thread
 * @param[in] name - name of thread
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
void __itt_thread_set_name (const(char)* name);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_thread_set_name_ptr__3_0_t = void function (const(char)* name);
extern __gshared __itt_thread_set_name_ptr__3_0_t __itt_thread_set_name_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @cond exclude_from_gpa_documentation */

/**
 * @brief Mark current thread as ignored from this point on, for the duration of its existence.
 */
void __itt_thread_ignore ();

/** @cond exclude_from_documentation */

alias __itt_thread_ignore_ptr__3_0_t = void function ();
extern __gshared __itt_thread_ignore_ptr__3_0_t __itt_thread_ignore_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} threads group */

/**
 * @defgroup suppress Error suppression
 * @ingroup public
 * General behavior: application continues to run, but errors are suppressed
 *
 * @{
 */

/*****************************************************************/ /**
* @name group of functions used for error suppression in correctness tools
*********************************************************************/
/** @{ */
/**
 * @hideinitializer
 * @brief possible value for suppression mask
 */


/**
 * @hideinitializer
 * @brief possible value for suppression mask (suppresses errors from threading analysis)
 */


/**
 * @hideinitializer
 * @brief possible value for suppression mask (suppresses errors from memory analysis)
 */


/**
 * @brief Start suppressing errors identified in mask on this thread
 */
void __itt_suppress_push (uint mask);

/** @cond exclude_from_documentation */

alias __itt_suppress_push_ptr__3_0_t = void function (uint mask);
extern __gshared __itt_suppress_push_ptr__3_0_t __itt_suppress_push_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Undo the effects of the matching call to __itt_suppress_push
 */
void __itt_suppress_pop ();

/** @cond exclude_from_documentation */

alias __itt_suppress_pop_ptr__3_0_t = void function ();
extern __gshared __itt_suppress_pop_ptr__3_0_t __itt_suppress_pop_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @enum __itt_model_disable
 * @brief Enumerator for the disable methods
 */
enum __itt_suppress_mode
{
    __itt_unsuppress_range = 0,
    __itt_suppress_range = 1
}

alias __itt_suppress_mode_t = __itt_suppress_mode;

/**
 * @brief Mark a range of memory for error suppression or unsuppression for error types included in mask
 */
void __itt_suppress_mark_range (__itt_suppress_mode_t mode, uint mask, void* address, size_t size);

/** @cond exclude_from_documentation */

alias __itt_suppress_mark_range_ptr__3_0_t = void function (__itt_suppress_mode_t mode, uint mask, void* address, size_t size);
extern __gshared __itt_suppress_mark_range_ptr__3_0_t __itt_suppress_mark_range_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Undo the effect of a matching call to __itt_suppress_mark_range.   If not matching
 *        call is found, nothing is changed.
 */
void __itt_suppress_clear_range (__itt_suppress_mode_t mode, uint mask, void* address, size_t size);

/** @cond exclude_from_documentation */

alias __itt_suppress_clear_range_ptr__3_0_t = void function (__itt_suppress_mode_t mode, uint mask, void* address, size_t size);
extern __gshared __itt_suppress_clear_range_ptr__3_0_t __itt_suppress_clear_range_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} */
/** @} suppress group */

/**
 * @defgroup sync Synchronization
 * @ingroup public
 * Indicate user-written synchronization code
 * @{
 */
/**
 * @hideinitializer
 * @brief possible value of attribute argument for sync object type
 */


/**
 * @hideinitializer
 * @brief possible value of attribute argument for sync object type
 */


/**
@brief Name a synchronization object
@param[in] addr       Handle for the synchronization object. You should
use a real address to uniquely identify the synchronization object.
@param[in] objtype    null-terminated object type string. If NULL is
passed, the name will be "User Synchronization".
@param[in] objname    null-terminated object name string. If NULL,
no name will be assigned to the object.
@param[in] attribute  one of [#__itt_attr_barrier, #__itt_attr_mutex]
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
void __itt_sync_create (void* addr, const(char)* objtype, const(char)* objname, int attribute);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_sync_create_ptr__3_0_t = void function (void* addr, const(char)* objtype, const(char)* objname, int attribute);
extern __gshared __itt_sync_create_ptr__3_0_t __itt_sync_create_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
@brief Rename a synchronization object

You can use the rename call to assign or reassign a name to a given
synchronization object.
@param[in] addr  handle for the synchronization object.
@param[in] name  null-terminated object name string.
*/

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
void __itt_sync_rename (void* addr, const(char)* name);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_sync_rename_ptr__3_0_t = void function (void* addr, const(char)* name);
extern __gshared __itt_sync_rename_ptr__3_0_t __itt_sync_rename_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 @brief Destroy a synchronization object.
 @param addr Handle for the synchronization object.
 */
void __itt_sync_destroy (void* addr);

/** @cond exclude_from_documentation */

alias __itt_sync_destroy_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_sync_destroy_ptr__3_0_t __itt_sync_destroy_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/*****************************************************************/ /**
* @name group of functions is used for performance measurement tools
*********************************************************************/
/** @{ */
/**
 * @brief Enter spin loop on user-defined sync object
 */
void __itt_sync_prepare (void* addr);

/** @cond exclude_from_documentation */

alias __itt_sync_prepare_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_sync_prepare_ptr__3_0_t __itt_sync_prepare_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Quit spin loop without acquiring spin object
 */
void __itt_sync_cancel (void* addr);

/** @cond exclude_from_documentation */

alias __itt_sync_cancel_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_sync_cancel_ptr__3_0_t __itt_sync_cancel_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Successful spin loop completion (sync object acquired)
 */
void __itt_sync_acquired (void* addr);

/** @cond exclude_from_documentation */

alias __itt_sync_acquired_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_sync_acquired_ptr__3_0_t __itt_sync_acquired_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Start sync object releasing code. Is called before the lock release call.
 */
void __itt_sync_releasing (void* addr);

/** @cond exclude_from_documentation */

alias __itt_sync_releasing_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_sync_releasing_ptr__3_0_t __itt_sync_releasing_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} */

/** @} sync group */

/**************************************************************/ /**
* @name group of functions is used for correctness checking tools
******************************************************************/
/** @{ */
/**
 * @ingroup legacy
 * @deprecated Legacy API
 * @brief Fast synchronization which does no require spinning.
 * - This special function is to be used by TBB and OpenMP libraries only when they know
 *   there is no spin but they need to suppress TC warnings about shared variable modifications.
 * - It only has corresponding pointers in static library and does not have corresponding function
 *   in dynamic library.
 * @see void __itt_sync_prepare(void* addr);
 */
void __itt_fsync_prepare (void* addr);

/** @cond exclude_from_documentation */

alias __itt_fsync_prepare_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_fsync_prepare_ptr__3_0_t __itt_fsync_prepare_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup legacy
 * @deprecated Legacy API
 * @brief Fast synchronization which does no require spinning.
 * - This special function is to be used by TBB and OpenMP libraries only when they know
 *   there is no spin but they need to suppress TC warnings about shared variable modifications.
 * - It only has corresponding pointers in static library and does not have corresponding function
 *   in dynamic library.
 * @see void __itt_sync_cancel(void *addr);
 */
void __itt_fsync_cancel (void* addr);

/** @cond exclude_from_documentation */

alias __itt_fsync_cancel_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_fsync_cancel_ptr__3_0_t __itt_fsync_cancel_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup legacy
 * @deprecated Legacy API
 * @brief Fast synchronization which does no require spinning.
 * - This special function is to be used by TBB and OpenMP libraries only when they know
 *   there is no spin but they need to suppress TC warnings about shared variable modifications.
 * - It only has corresponding pointers in static library and does not have corresponding function
 *   in dynamic library.
 * @see void __itt_sync_acquired(void *addr);
 */
void __itt_fsync_acquired (void* addr);

/** @cond exclude_from_documentation */

alias __itt_fsync_acquired_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_fsync_acquired_ptr__3_0_t __itt_fsync_acquired_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup legacy
 * @deprecated Legacy API
 * @brief Fast synchronization which does no require spinning.
 * - This special function is to be used by TBB and OpenMP libraries only when they know
 *   there is no spin but they need to suppress TC warnings about shared variable modifications.
 * - It only has corresponding pointers in static library and does not have corresponding function
 *   in dynamic library.
 * @see void __itt_sync_releasing(void* addr);
 */
void __itt_fsync_releasing (void* addr);

/** @cond exclude_from_documentation */

alias __itt_fsync_releasing_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_fsync_releasing_ptr__3_0_t __itt_fsync_releasing_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} */

/**
 * @defgroup model Modeling by Intel(R) Parallel Advisor
 * @ingroup public
 * This is the subset of itt used for modeling by Intel(R) Parallel Advisor.
 * This API is called ONLY using annotate.h, by "Annotation" macros
 * the user places in their sources during the parallelism modeling steps.
 *
 * site_begin/end and task_begin/end take the address of handle variables,
 * which are writeable by the API.  Handles must be 0 initialized prior
 * to the first call to begin, or may cause a run-time failure.
 * The handles are initialized in a multi-thread safe way by the API if
 * the handle is 0.  The commonly expected idiom is one static handle to
 * identify a site or task.  If a site or task of the same name has already
 * been started during this collection, the same handle MAY be returned,
 * but is not required to be - it is unspecified if data merging is done
 * based on name.  These routines also take an instance variable.  Like
 * the lexical instance, these must be 0 initialized.  Unlike the lexical
 * instance, this is used to track a single dynamic instance.
 *
 * API used by the Intel(R) Parallel Advisor to describe potential concurrency
 * and related activities. User-added source annotations expand to calls
 * to these procedures to enable modeling of a hypothetical concurrent
 * execution serially.
 * @{
 */

alias __itt_model_site = void*; /*!< @brief handle for lexical site     */
alias __itt_model_site_instance = void*; /*!< @brief handle for dynamic instance */
alias __itt_model_task = void*; /*!< @brief handle for lexical site     */
alias __itt_model_task_instance = void*; /*!< @brief handle for dynamic instance */

/**
 * @enum __itt_model_disable
 * @brief Enumerator for the disable methods
 */
enum __itt_model_disable
{
    __itt_model_disable_observation = 0,
    __itt_model_disable_collection = 1
}

/* !_ADVISOR_ANNOTATE_H_ || ANNOTATE_EXPAND_NULL */

/**
 * @brief ANNOTATE_SITE_BEGIN/ANNOTATE_SITE_END support.
 *
 * site_begin/end model a potential concurrency site.
 * site instances may be recursively nested with themselves.
 * site_end exits the most recently started but unended site for the current
 * thread.  The handle passed to end may be used to validate structure.
 * Instances of a site encountered on different threads concurrently
 * are considered completely distinct. If the site name for two different
 * lexical sites match, it is unspecified whether they are treated as the
 * same or different for data presentation.
 */
void __itt_model_site_begin (__itt_model_site* site, __itt_model_site_instance* instance, const(char)* name);

void __itt_model_site_beginA (const(char)* name);
void __itt_model_site_beginAL (const(char)* name, size_t siteNameLen);
void __itt_model_site_end (__itt_model_site* site, __itt_model_site_instance* instance);
void __itt_model_site_end_2 ();

/** @cond exclude_from_documentation */

alias __itt_model_site_begin_ptr__3_0_t = void function (__itt_model_site* site, __itt_model_site_instance* instance, const(char)* name);
extern __gshared __itt_model_site_begin_ptr__3_0_t __itt_model_site_begin_ptr__3_0;

alias __itt_model_site_beginA_ptr__3_0_t = void function (const(char)* name);
extern __gshared __itt_model_site_beginA_ptr__3_0_t __itt_model_site_beginA_ptr__3_0;
alias __itt_model_site_beginAL_ptr__3_0_t = void function (const(char)* name, size_t siteNameLen);
extern __gshared __itt_model_site_beginAL_ptr__3_0_t __itt_model_site_beginAL_ptr__3_0;
alias __itt_model_site_end_ptr__3_0_t = void function (__itt_model_site* site, __itt_model_site_instance* instance);
extern __gshared __itt_model_site_end_ptr__3_0_t __itt_model_site_end_ptr__3_0;
alias __itt_model_site_end_2_ptr__3_0_t = void function ();
extern __gshared __itt_model_site_end_2_ptr__3_0_t __itt_model_site_end_2_ptr__3_0;











/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_TASK_BEGIN/ANNOTATE_TASK_END support
 *
 * task_begin/end model a potential task, which is contained within the most
 * closely enclosing dynamic site.  task_end exits the most recently started
 * but unended task.  The handle passed to end may be used to validate
 * structure.  It is unspecified if bad dynamic nesting is detected.  If it
 * is, it should be encoded in the resulting data collection.  The collector
 * should not fail due to construct nesting issues, nor attempt to directly
 * indicate the problem.
 */
void __itt_model_task_begin (__itt_model_task* task, __itt_model_task_instance* instance, const(char)* name);

void __itt_model_task_beginA (const(char)* name);
void __itt_model_task_beginAL (const(char)* name, size_t taskNameLen);
void __itt_model_iteration_taskA (const(char)* name);
void __itt_model_iteration_taskAL (const(char)* name, size_t taskNameLen);
void __itt_model_task_end (__itt_model_task* task, __itt_model_task_instance* instance);
void __itt_model_task_end_2 ();

/** @cond exclude_from_documentation */

alias __itt_model_task_begin_ptr__3_0_t = void function (__itt_model_task* task, __itt_model_task_instance* instance, const(char)* name);
extern __gshared __itt_model_task_begin_ptr__3_0_t __itt_model_task_begin_ptr__3_0;

alias __itt_model_task_beginA_ptr__3_0_t = void function (const(char)* name);
extern __gshared __itt_model_task_beginA_ptr__3_0_t __itt_model_task_beginA_ptr__3_0;
alias __itt_model_task_beginAL_ptr__3_0_t = void function (const(char)* name, size_t taskNameLen);
extern __gshared __itt_model_task_beginAL_ptr__3_0_t __itt_model_task_beginAL_ptr__3_0;
alias __itt_model_iteration_taskA_ptr__3_0_t = void function (const(char)* name);
extern __gshared __itt_model_iteration_taskA_ptr__3_0_t __itt_model_iteration_taskA_ptr__3_0;
alias __itt_model_iteration_taskAL_ptr__3_0_t = void function (const(char)* name, size_t taskNameLen);
extern __gshared __itt_model_iteration_taskAL_ptr__3_0_t __itt_model_iteration_taskAL_ptr__3_0;
alias __itt_model_task_end_ptr__3_0_t = void function (__itt_model_task* task, __itt_model_task_instance* instance);
extern __gshared __itt_model_task_end_ptr__3_0_t __itt_model_task_end_ptr__3_0;
alias __itt_model_task_end_2_ptr__3_0_t = void function ();
extern __gshared __itt_model_task_end_2_ptr__3_0_t __itt_model_task_end_2_ptr__3_0;















/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_LOCK_ACQUIRE/ANNOTATE_LOCK_RELEASE support
 *
 * lock_acquire/release model a potential lock for both lockset and
 * performance modeling.  Each unique address is modeled as a separate
 * lock, with invalid addresses being valid lock IDs.  Specifically:
 * no storage is accessed by the API at the specified address - it is only
 * used for lock identification.  Lock acquires may be self-nested and are
 * unlocked by a corresponding number of releases.
 * (These closely correspond to __itt_sync_acquired/__itt_sync_releasing,
 * but may not have identical semantics.)
 */
void __itt_model_lock_acquire (void* lock);
void __itt_model_lock_acquire_2 (void* lock);
void __itt_model_lock_release (void* lock);
void __itt_model_lock_release_2 (void* lock);

/** @cond exclude_from_documentation */

alias __itt_model_lock_acquire_ptr__3_0_t = void function (void* lock);
extern __gshared __itt_model_lock_acquire_ptr__3_0_t __itt_model_lock_acquire_ptr__3_0;
alias __itt_model_lock_acquire_2_ptr__3_0_t = void function (void* lock);
extern __gshared __itt_model_lock_acquire_2_ptr__3_0_t __itt_model_lock_acquire_2_ptr__3_0;
alias __itt_model_lock_release_ptr__3_0_t = void function (void* lock);
extern __gshared __itt_model_lock_release_ptr__3_0_t __itt_model_lock_release_ptr__3_0;
alias __itt_model_lock_release_2_ptr__3_0_t = void function (void* lock);
extern __gshared __itt_model_lock_release_2_ptr__3_0_t __itt_model_lock_release_2_ptr__3_0;








/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_RECORD_ALLOCATION/ANNOTATE_RECORD_DEALLOCATION support
 *
 * record_allocation/deallocation describe user-defined memory allocator
 * behavior, which may be required for correctness modeling to understand
 * when storage is not expected to be actually reused across threads.
 */
void __itt_model_record_allocation (void* addr, size_t size);
void __itt_model_record_deallocation (void* addr);

/** @cond exclude_from_documentation */

alias __itt_model_record_allocation_ptr__3_0_t = void function (void* addr, size_t size);
extern __gshared __itt_model_record_allocation_ptr__3_0_t __itt_model_record_allocation_ptr__3_0;
alias __itt_model_record_deallocation_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_model_record_deallocation_ptr__3_0_t __itt_model_record_deallocation_ptr__3_0;




/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_INDUCTION_USES support
 *
 * Note particular storage is inductive through the end of the current site
 */
void __itt_model_induction_uses (void* addr, size_t size);

/** @cond exclude_from_documentation */

alias __itt_model_induction_uses_ptr__3_0_t = void function (void* addr, size_t size);
extern __gshared __itt_model_induction_uses_ptr__3_0_t __itt_model_induction_uses_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_REDUCTION_USES support
 *
 * Note particular storage is used for reduction through the end
 * of the current site
 */
void __itt_model_reduction_uses (void* addr, size_t size);

/** @cond exclude_from_documentation */

alias __itt_model_reduction_uses_ptr__3_0_t = void function (void* addr, size_t size);
extern __gshared __itt_model_reduction_uses_ptr__3_0_t __itt_model_reduction_uses_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_OBSERVE_USES support
 *
 * Have correctness modeling record observations about uses of storage
 * through the end of the current site
 */
void __itt_model_observe_uses (void* addr, size_t size);

/** @cond exclude_from_documentation */

alias __itt_model_observe_uses_ptr__3_0_t = void function (void* addr, size_t size);
extern __gshared __itt_model_observe_uses_ptr__3_0_t __itt_model_observe_uses_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_CLEAR_USES support
 *
 * Clear the special handling of a piece of storage related to induction,
 * reduction or observe_uses
 */
void __itt_model_clear_uses (void* addr);

/** @cond exclude_from_documentation */

alias __itt_model_clear_uses_ptr__3_0_t = void function (void* addr);
extern __gshared __itt_model_clear_uses_ptr__3_0_t __itt_model_clear_uses_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief ANNOTATE_DISABLE_*_PUSH/ANNOTATE_DISABLE_*_POP support
 *
 * disable_push/disable_pop push and pop disabling based on a parameter.
 * Disabling observations stops processing of memory references during
 * correctness modeling, and all annotations that occur in the disabled
 * region.  This allows description of code that is expected to be handled
 * specially during conversion to parallelism or that is not recognized
 * by tools (e.g. some kinds of synchronization operations.)
 * This mechanism causes all annotations in the disabled region, other
 * than disable_push and disable_pop, to be ignored.  (For example, this
 * might validly be used to disable an entire parallel site and the contained
 * tasks and locking in it for data collection purposes.)
 * The disable for collection is a more expensive operation, but reduces
 * collector overhead significantly.  This applies to BOTH correctness data
 * collection and performance data collection.  For example, a site
 * containing a task might only enable data collection for the first 10
 * iterations.  Both performance and correctness data should reflect this,
 * and the program should run as close to full speed as possible when
 * collection is disabled.
 */
void __itt_model_disable_push (__itt_model_disable x);
void __itt_model_disable_pop ();
void __itt_model_aggregate_task (size_t x);

/** @cond exclude_from_documentation */

alias __itt_model_disable_push_ptr__3_0_t = void function (__itt_model_disable x);
extern __gshared __itt_model_disable_push_ptr__3_0_t __itt_model_disable_push_ptr__3_0;
alias __itt_model_disable_pop_ptr__3_0_t = void function ();
extern __gshared __itt_model_disable_pop_ptr__3_0_t __itt_model_disable_pop_ptr__3_0;
alias __itt_model_aggregate_task_ptr__3_0_t = void function (size_t x);
extern __gshared __itt_model_aggregate_task_ptr__3_0_t __itt_model_aggregate_task_ptr__3_0;






/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} model group */

/**
 * @defgroup heap Heap
 * @ingroup public
 * Heap group
 * @{
 */

alias __itt_heap_function = void*;

/**
 * @brief Create an identification for heap function
 * @return non-zero identifier or NULL
 */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
__itt_heap_function __itt_heap_function_create (const(char)* name, const(char)* domain);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_heap_function_create_ptr__3_0_t = void* function (const(char)* name, const(char)* domain);
extern __gshared __itt_heap_function_create_ptr__3_0_t __itt_heap_function_create_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an allocation begin occurrence.
 */
void __itt_heap_allocate_begin (__itt_heap_function h, size_t size, int initialized);

/** @cond exclude_from_documentation */

alias __itt_heap_allocate_begin_ptr__3_0_t = void function (__itt_heap_function h, size_t size, int initialized);
extern __gshared __itt_heap_allocate_begin_ptr__3_0_t __itt_heap_allocate_begin_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an allocation end occurrence.
 */
void __itt_heap_allocate_end (__itt_heap_function h, void** addr, size_t size, int initialized);

/** @cond exclude_from_documentation */

alias __itt_heap_allocate_end_ptr__3_0_t = void function (__itt_heap_function h, void** addr, size_t size, int initialized);
extern __gshared __itt_heap_allocate_end_ptr__3_0_t __itt_heap_allocate_end_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an free begin occurrence.
 */
void __itt_heap_free_begin (__itt_heap_function h, void* addr);

/** @cond exclude_from_documentation */

alias __itt_heap_free_begin_ptr__3_0_t = void function (__itt_heap_function h, void* addr);
extern __gshared __itt_heap_free_begin_ptr__3_0_t __itt_heap_free_begin_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an free end occurrence.
 */
void __itt_heap_free_end (__itt_heap_function h, void* addr);

/** @cond exclude_from_documentation */

alias __itt_heap_free_end_ptr__3_0_t = void function (__itt_heap_function h, void* addr);
extern __gshared __itt_heap_free_end_ptr__3_0_t __itt_heap_free_end_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an reallocation begin occurrence.
 */
void __itt_heap_reallocate_begin (__itt_heap_function h, void* addr, size_t new_size, int initialized);

/** @cond exclude_from_documentation */

alias __itt_heap_reallocate_begin_ptr__3_0_t = void function (__itt_heap_function h, void* addr, size_t new_size, int initialized);
extern __gshared __itt_heap_reallocate_begin_ptr__3_0_t __itt_heap_reallocate_begin_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an reallocation end occurrence.
 */
void __itt_heap_reallocate_end (__itt_heap_function h, void* addr, void** new_addr, size_t new_size, int initialized);

/** @cond exclude_from_documentation */

alias __itt_heap_reallocate_end_ptr__3_0_t = void function (__itt_heap_function h, void* addr, void** new_addr, size_t new_size, int initialized);
extern __gshared __itt_heap_reallocate_end_ptr__3_0_t __itt_heap_reallocate_end_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @brief internal access begin */
void __itt_heap_internal_access_begin ();

/** @cond exclude_from_documentation */

alias __itt_heap_internal_access_begin_ptr__3_0_t = void function ();
extern __gshared __itt_heap_internal_access_begin_ptr__3_0_t __itt_heap_internal_access_begin_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @brief internal access end */
void __itt_heap_internal_access_end ();

/** @cond exclude_from_documentation */

alias __itt_heap_internal_access_end_ptr__3_0_t = void function ();
extern __gshared __itt_heap_internal_access_end_ptr__3_0_t __itt_heap_internal_access_end_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @brief record memory growth begin */
void __itt_heap_record_memory_growth_begin ();

/** @cond exclude_from_documentation */

alias __itt_heap_record_memory_growth_begin_ptr__3_0_t = void function ();
extern __gshared __itt_heap_record_memory_growth_begin_ptr__3_0_t __itt_heap_record_memory_growth_begin_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @brief record memory growth end */
void __itt_heap_record_memory_growth_end ();

/** @cond exclude_from_documentation */

alias __itt_heap_record_memory_growth_end_ptr__3_0_t = void function ();
extern __gshared __itt_heap_record_memory_growth_end_ptr__3_0_t __itt_heap_record_memory_growth_end_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Specify the type of heap detection/reporting to modify.
 */
/**
 * @hideinitializer
 * @brief Report on memory leaks.
 */


/**
 * @hideinitializer
 * @brief Report on memory growth.
 */


/** @brief heap reset detection */
void __itt_heap_reset_detection (uint reset_mask);

/** @cond exclude_from_documentation */

alias __itt_heap_reset_detection_ptr__3_0_t = void function (uint reset_mask);
extern __gshared __itt_heap_reset_detection_ptr__3_0_t __itt_heap_reset_detection_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @brief report */
void __itt_heap_record (uint record_mask);

/** @cond exclude_from_documentation */

alias __itt_heap_record_ptr__3_0_t = void function (uint record_mask);
extern __gshared __itt_heap_record_ptr__3_0_t __itt_heap_record_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @} heap group */
/** @endcond */
/* ========================================================================== */

/**
 * @defgroup domains Domains
 * @ingroup public
 * Domains group
 * @{
 */

/** @cond exclude_from_documentation */

struct ___itt_domain
{
    int flags; /*!< Zero if disabled, non-zero if enabled. The meaning of different non-zero values is reserved to the runtime */
    const(char)* nameA; /*!< Copy of original name in ASCII. */

    /*!< Copy of original name in UNICODE. */
    /* UNICODE || _UNICODE */
    void* nameW;
    /* UNICODE || _UNICODE */
    int extra1; /*!< Reserved to the runtime */
    void* extra2; /*!< Reserved to the runtime */
    ___itt_domain* next;
}

alias __itt_domain = ___itt_domain;

/** @endcond */

/**
 * @ingroup domains
 * @brief Create a domain.
 * Create domain using some domain name: the URI naming style is recommended.
 * Because the set of domains is expected to be static over the application's
 * execution time, there is no mechanism to destroy a domain.
 * Any domain can be accessed by any thread in the process, regardless of
 * which thread created the domain. This call is thread-safe.
 * @param[in] name name of domain
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
__itt_domain* __itt_domain_create (const(char)* name);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_domain_create_ptr__3_0_t = ___itt_domain* function (const(char)* name);
extern __gshared __itt_domain_create_ptr__3_0_t __itt_domain_create_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} domains group */

/**
 * @defgroup ids IDs
 * @ingroup public
 * IDs group
 * @{
 */

/** @cond exclude_from_documentation */

struct ___itt_id
{
    ulong d1;
    ulong d2;
    ulong d3;
}

alias __itt_id = ___itt_id;

/** @endcond */

extern __gshared const __itt_id __itt_null;

/**
 * @ingroup ids
 * @brief A convenience function is provided to create an ID without domain control.
 * @brief This is a convenience function to initialize an __itt_id structure. This function
 * does not affect the collector runtime in any way. After you make the ID with this
 * function, you still must create it with the __itt_id_create function before using the ID
 * to identify a named entity.
 * @param[in] addr The address of object; high QWORD of the ID value.
 * @param[in] extra The extra data to unique identify object; low QWORD of the ID value.
 */

__itt_id __itt_id_make (void* addr, ulong extra);

/* Reserved. Must be zero */
__itt_id __itt_id_make (void* addr, ulong extra);

/**
 * @ingroup ids
 * @brief Create an instance of identifier.
 * This establishes the beginning of the lifetime of an instance of
 * the given ID in the trace. Once this lifetime starts, the ID
 * can be used to tag named entity instances in calls such as
 * __itt_task_begin, and to specify relationships among
 * identified named entity instances, using the \ref relations APIs.
 * Instance IDs are not domain specific!
 * @param[in] domain The domain controlling the execution of this call.
 * @param[in] id The ID to create.
 */
void __itt_id_create (const(__itt_domain)* domain, __itt_id id);

/** @cond exclude_from_documentation */

alias __itt_id_create_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id);
extern __gshared __itt_id_create_ptr__3_0_t __itt_id_create_ptr__3_0;

extern (D) auto __itt_id_create(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(id_create, d, x);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup ids
 * @brief Destroy an instance of identifier.
 * This ends the lifetime of the current instance of the given ID value in the trace.
 * Any relationships that are established after this lifetime ends are invalid.
 * This call must be performed before the given ID value can be reused for a different
 * named entity instance.
 * @param[in] domain The domain controlling the execution of this call.
 * @param[in] id The ID to destroy.
 */
void __itt_id_destroy (const(__itt_domain)* domain, __itt_id id);

/** @cond exclude_from_documentation */

alias __itt_id_destroy_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id);
extern __gshared __itt_id_destroy_ptr__3_0_t __itt_id_destroy_ptr__3_0;

extern (D) auto __itt_id_destroy(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(id_destroy, d, x);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} ids group */

/**
 * @defgroup handless String Handles
 * @ingroup public
 * String Handles group
 * @{
 */

/** @cond exclude_from_documentation */

struct ___itt_string_handle
{
    const(char)* strA; /*!< Copy of original string in ASCII. */

    /*!< Copy of original string in UNICODE. */
    /* UNICODE || _UNICODE */
    void* strW;
    /* UNICODE || _UNICODE */
    int extra1; /*!< Reserved. Must be zero   */
    void* extra2; /*!< Reserved. Must be zero   */
    ___itt_string_handle* next;
}

alias __itt_string_handle = ___itt_string_handle;

/** @endcond */

/**
 * @ingroup handles
 * @brief Create a string handle.
 * Create and return handle value that can be associated with a string.
 * Consecutive calls to __itt_string_handle_create with the same name
 * return the same value. Because the set of string handles is expected to remain
 * static during the application's execution time, there is no mechanism to destroy a string handle.
 * Any string handle can be accessed by any thread in the process, regardless of which thread created
 * the string handle. This call is thread-safe.
 * @param[in] name The input string
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
__itt_string_handle* __itt_string_handle_create (const(char)* name);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_string_handle_create_ptr__3_0_t = ___itt_string_handle* function (const(char)* name);
extern __gshared __itt_string_handle_create_ptr__3_0_t __itt_string_handle_create_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} handles group */

/** @cond exclude_from_documentation */
alias __itt_timestamp = ulong;
/** @endcond */



/** @cond exclude_from_gpa_documentation */

/**
 * @ingroup timestamps
 * @brief Return timestamp corresponding to the current moment.
 * This returns the timestamp in the format that is the most relevant for the current
 * host or platform (RDTSC, QPC, and others). You can use the "<" operator to
 * compare __itt_timestamp values.
 */
__itt_timestamp __itt_get_timestamp ();

/** @cond exclude_from_documentation */

alias __itt_get_timestamp_ptr__3_0_t = ulong function ();
extern __gshared __itt_get_timestamp_ptr__3_0_t __itt_get_timestamp_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} timestamps */
/** @endcond */

/** @cond exclude_from_gpa_documentation */

/**
 * @defgroup regions Regions
 * @ingroup public
 * Regions group
 * @{
 */
/**
 * @ingroup regions
 * @brief Begin of region instance.
 * Successive calls to __itt_region_begin with the same ID are ignored
 * until a call to __itt_region_end with the same ID
 * @param[in] domain The domain for this region instance
 * @param[in] id The instance ID for this region instance. Must not be __itt_null
 * @param[in] parentid The instance ID for the parent of this region instance, or __itt_null
 * @param[in] name The name of this region
 */
void __itt_region_begin (const(__itt_domain)* domain, __itt_id id, __itt_id parentid, __itt_string_handle* name);

/**
 * @ingroup regions
 * @brief End of region instance.
 * The first call to __itt_region_end with a given ID ends the
 * region. Successive calls with the same ID are ignored, as are
 * calls that do not have a matching __itt_region_begin call.
 * @param[in] domain The domain for this region instance
 * @param[in] id The instance ID for this region instance
 */
void __itt_region_end (const(__itt_domain)* domain, __itt_id id);

/** @cond exclude_from_documentation */

alias __itt_region_begin_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_id parentid, __itt_string_handle* name);
extern __gshared __itt_region_begin_ptr__3_0_t __itt_region_begin_ptr__3_0;
alias __itt_region_end_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id);
extern __gshared __itt_region_end_ptr__3_0_t __itt_region_end_ptr__3_0;

extern (D) auto __itt_region_begin(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(region_begin, d, x, y, z);
}



extern (D) auto __itt_region_end(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(region_end, d, x);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} regions group */

/**
 * @defgroup frames Frames
 * @ingroup public
 * Frames are similar to regions, but are intended to be easier to use and to implement.
 * In particular:
 * - Frames always represent periods of elapsed time
 * - By default, frames have no nesting relationships
 * @{
 */

/**
 * @ingroup frames
 * @brief Begin a frame instance.
 * Successive calls to __itt_frame_begin with the
 * same ID are ignored until a call to __itt_frame_end with the same ID.
 * @param[in] domain The domain for this frame instance
 * @param[in] id The instance ID for this frame instance or NULL
 */
void __itt_frame_begin_v3 (const(__itt_domain)* domain, __itt_id* id);

/**
 * @ingroup frames
 * @brief End a frame instance.
 * The first call to __itt_frame_end with a given ID
 * ends the frame. Successive calls with the same ID are ignored, as are
 * calls that do not have a matching __itt_frame_begin call.
 * @param[in] domain The domain for this frame instance
 * @param[in] id The instance ID for this frame instance or NULL for current
 */
void __itt_frame_end_v3 (const(__itt_domain)* domain, __itt_id* id);

/**
 * @ingroup frames
 * @brief Submits a frame instance.
 * Successive calls to __itt_frame_begin or __itt_frame_submit with the
 * same ID are ignored until a call to __itt_frame_end or __itt_frame_submit
 * with the same ID.
 * Passing special __itt_timestamp_none value as "end" argument means
 * take the current timestamp as the end timestamp.
 * @param[in] domain The domain for this frame instance
 * @param[in] id The instance ID for this frame instance or NULL
 * @param[in] begin Timestamp of the beginning of the frame
 * @param[in] end Timestamp of the end of the frame
 */
void __itt_frame_submit_v3 (
    const(__itt_domain)* domain,
    __itt_id* id,
    __itt_timestamp begin,
    __itt_timestamp end);

/** @cond exclude_from_documentation */

alias __itt_frame_begin_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id* id);
extern __gshared __itt_frame_begin_v3_ptr__3_0_t __itt_frame_begin_v3_ptr__3_0;
alias __itt_frame_end_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id* id);
extern __gshared __itt_frame_end_v3_ptr__3_0_t __itt_frame_end_v3_ptr__3_0;
alias __itt_frame_submit_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id* id, __itt_timestamp begin, __itt_timestamp end);
extern __gshared __itt_frame_submit_v3_ptr__3_0_t __itt_frame_submit_v3_ptr__3_0;

extern (D) auto __itt_frame_begin_v3(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(frame_begin_v3, d, x);
}



extern (D) auto __itt_frame_end_v3(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(frame_end_v3, d, x);
}



extern (D) auto __itt_frame_submit_v3(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 b, auto ref T3 e)
{
    return ITTNOTIFY_VOID_D3(frame_submit_v3, d, x, b, e);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} frames group */
/** @endcond */

/**
 * @defgroup taskgroup Task Group
 * @ingroup public
 * Task Group
 * @{
 */
/**
 * @ingroup task_groups
 * @brief Denotes a task_group instance.
 * Successive calls to __itt_task_group with the same ID are ignored.
 * @param[in] domain The domain for this task_group instance
 * @param[in] id The instance ID for this task_group instance. Must not be __itt_null.
 * @param[in] parentid The instance ID for the parent of this task_group instance, or __itt_null.
 * @param[in] name The name of this task_group
 */
void __itt_task_group (const(__itt_domain)* domain, __itt_id id, __itt_id parentid, __itt_string_handle* name);

/** @cond exclude_from_documentation */

alias __itt_task_group_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_id parentid, __itt_string_handle* name);
extern __gshared __itt_task_group_ptr__3_0_t __itt_task_group_ptr__3_0;

extern (D) auto __itt_task_group(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(task_group, d, x, y, z);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} taskgroup group */

/**
 * @defgroup tasks Tasks
 * @ingroup public
 * A task instance represents a piece of work performed by a particular
 * thread for a period of time. A call to __itt_task_begin creates a
 * task instance. This becomes the current instance for that task on that
 * thread. A following call to __itt_task_end on the same thread ends the
 * instance. There may be multiple simultaneous instances of tasks with the
 * same name on different threads. If an ID is specified, the task instance
 * receives that ID. Nested tasks are allowed.
 *
 * Note: The task is defined by the bracketing of __itt_task_begin and
 * __itt_task_end on the same thread. If some scheduling mechanism causes
 * task switching (the thread executes a different user task) or task
 * switching (the user task switches to a different thread) then this breaks
 * the notion of  current instance. Additional API calls are required to
 * deal with that possibility.
 * @{
 */

/**
 * @ingroup tasks
 * @brief Begin a task instance.
 * @param[in] domain The domain for this task
 * @param[in] taskid The instance ID for this task instance, or __itt_null
 * @param[in] parentid The parent instance to which this task instance belongs, or __itt_null
 * @param[in] name The name of this task
 */
void __itt_task_begin (const(__itt_domain)* domain, __itt_id taskid, __itt_id parentid, __itt_string_handle* name);

/**
 * @ingroup tasks
 * @brief Begin a task instance.
 * @param[in] domain The domain for this task
 * @param[in] taskid The identifier for this task instance (may be 0)
 * @param[in] parentid The parent of this task (may be 0)
 * @param[in] fn The pointer to the function you are tracing
 */
void __itt_task_begin_fn (const(__itt_domain)* domain, __itt_id taskid, __itt_id parentid, void* fn);

/**
 * @ingroup tasks
 * @brief End the current task instance.
 * @param[in] domain The domain for this task
 */
void __itt_task_end (const(__itt_domain)* domain);

/**
 * @ingroup tasks
 * @brief Begin an overlapped task instance.
 * @param[in] domain The domain for this task.
 * @param[in] taskid The identifier for this task instance, *cannot* be __itt_null.
 * @param[in] parentid The parent of this task, or __itt_null.
 * @param[in] name The name of this task.
 */
void __itt_task_begin_overlapped (const(__itt_domain)* domain, __itt_id taskid, __itt_id parentid, __itt_string_handle* name);

/**
 * @ingroup tasks
 * @brief End an overlapped task instance.
 * @param[in] domain The domain for this task
 * @param[in] taskid Explicit ID of finished task
 */
void __itt_task_end_overlapped (const(__itt_domain)* domain, __itt_id taskid);

/** @cond exclude_from_documentation */

alias __itt_task_begin_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_id parentid, __itt_string_handle* name);
extern __gshared __itt_task_begin_ptr__3_0_t __itt_task_begin_ptr__3_0;
alias __itt_task_begin_fn_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_id parentid, void* fn);
extern __gshared __itt_task_begin_fn_ptr__3_0_t __itt_task_begin_fn_ptr__3_0;
alias __itt_task_end_ptr__3_0_t = void function (const(__itt_domain)* domain);
extern __gshared __itt_task_end_ptr__3_0_t __itt_task_end_ptr__3_0;
alias __itt_task_begin_overlapped_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id taskid, __itt_id parentid, __itt_string_handle* name);
extern __gshared __itt_task_begin_overlapped_ptr__3_0_t __itt_task_begin_overlapped_ptr__3_0;
alias __itt_task_end_overlapped_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id taskid);
extern __gshared __itt_task_end_overlapped_ptr__3_0_t __itt_task_end_overlapped_ptr__3_0;

extern (D) auto __itt_task_begin(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(task_begin, d, x, y, z);
}



extern (D) auto __itt_task_begin_fn(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(task_begin_fn, d, x, y, z);
}



extern (D) auto __itt_task_end(T)(auto ref T d)
{
    return ITTNOTIFY_VOID_D0(task_end, d);
}



extern (D) auto __itt_task_begin_overlapped(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(task_begin_overlapped, d, x, y, z);
}



extern (D) auto __itt_task_end_overlapped(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(task_end_overlapped, d, x);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} tasks group */

/**
 * @defgroup markers Markers
 * Markers represent a single discreet event in time. Markers have a scope,
 * described by an enumerated type __itt_scope. Markers are created by
 * the API call __itt_marker. A marker instance can be given an ID for use in
 * adding metadata.
 * @{
 */

/**
 * @brief Describes the scope of an event object in the trace.
 */
enum __itt_scope
{
    __itt_scope_unknown = 0,
    __itt_scope_global = 1,
    __itt_scope_track_group = 2,
    __itt_scope_track = 3,
    __itt_scope_task = 4,
    __itt_scope_marker = 5
}

/** @cond exclude_from_documentation */





/** @endcond */

/**
 * @ingroup markers
 * @brief Create a marker instance
 * @param[in] domain The domain for this marker
 * @param[in] id The instance ID for this marker or __itt_null
 * @param[in] name The name for this marker
 * @param[in] scope The scope for this marker
 */
void __itt_marker (const(__itt_domain)* domain, __itt_id id, __itt_string_handle* name, __itt_scope scope_);

/** @cond exclude_from_documentation */

alias __itt_marker_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_string_handle* name, __itt_scope scope_);
extern __gshared __itt_marker_ptr__3_0_t __itt_marker_ptr__3_0;

extern (D) auto __itt_marker(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(marker, d, x, y, z);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} markers group */

/**
 * @defgroup metadata Metadata
 * The metadata API is used to attach extra information to named
 * entities. Metadata can be attached to an identified named entity by ID,
 * or to the current entity (which is always a task).
 *
 * Conceptually metadata has a type (what kind of metadata), a key (the
 * name of the metadata), and a value (the actual data). The encoding of
 * the value depends on the type of the metadata.
 *
 * The type of metadata is specified by an enumerated type __itt_metdata_type.
 * @{
 */

/**
 * @ingroup parameters
 * @brief describes the type of metadata
 */
enum __itt_metadata_type
{
    __itt_metadata_unknown = 0,
    __itt_metadata_u64 = 1, /**< Unsigned 64-bit integer */
    __itt_metadata_s64 = 2, /**< Signed 64-bit integer */
    __itt_metadata_u32 = 3, /**< Unsigned 32-bit integer */
    __itt_metadata_s32 = 4, /**< Signed 32-bit integer */
    __itt_metadata_u16 = 5, /**< Unsigned 16-bit integer */
    __itt_metadata_s16 = 6, /**< Signed 16-bit integer */
    __itt_metadata_float = 7, /**< Signed 32-bit floating-point */
    __itt_metadata_double = 8 /**< SIgned 64-bit floating-point */
}

/**
 * @ingroup parameters
 * @brief Add metadata to an instance of a named entity.
 * @param[in] domain The domain controlling the call
 * @param[in] id The identifier of the instance to which the metadata is to be added, or __itt_null to add to the current task
 * @param[in] key The name of the metadata
 * @param[in] type The type of the metadata
 * @param[in] count The number of elements of the given type. If count == 0, no metadata will be added.
 * @param[in] data The metadata itself
*/
void __itt_metadata_add (const(__itt_domain)* domain, __itt_id id, __itt_string_handle* key, __itt_metadata_type type, size_t count, void* data);

/** @cond exclude_from_documentation */

alias __itt_metadata_add_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_string_handle* key, __itt_metadata_type type, size_t count, void* data);
extern __gshared __itt_metadata_add_ptr__3_0_t __itt_metadata_add_ptr__3_0;

extern (D) auto __itt_metadata_add(T0, T1, T2, T3, T4, T5)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a, auto ref T5 b)
{
    return ITTNOTIFY_VOID_D5(metadata_add, d, x, y, z, a, b);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup parameters
 * @brief Add string metadata to an instance of a named entity.
 * @param[in] domain The domain controlling the call
 * @param[in] id The identifier of the instance to which the metadata is to be added, or __itt_null to add to the current task
 * @param[in] key The name of the metadata
 * @param[in] data The metadata itself
 * @param[in] length The number of characters in the string, or -1 if the length is unknown but the string is null-terminated
*/

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
void __itt_metadata_str_add (const(__itt_domain)* domain, __itt_id id, __itt_string_handle* key, const(char)* data, size_t length);

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_metadata_str_add_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id id, __itt_string_handle* key, const(char)* data, size_t length);
extern __gshared __itt_metadata_str_add_ptr__3_0_t __itt_metadata_str_add_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
extern (D) auto __itt_metadata_str_add(T0, T1, T2, T3, T4)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a)
{
    return ITTNOTIFY_VOID_D4(metadata_str_add, d, x, y, z, a);
}


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup parameters
 * @brief Add metadata to an instance of a named entity.
 * @param[in] domain The domain controlling the call
 * @param[in] scope The scope of the instance to which the metadata is to be added

 * @param[in] id The identifier of the instance to which the metadata is to be added, or __itt_null to add to the current task

 * @param[in] key The name of the metadata
 * @param[in] type The type of the metadata
 * @param[in] count The number of elements of the given type. If count == 0, no metadata will be added.
 * @param[in] data The metadata itself
*/
void __itt_metadata_add_with_scope (const(__itt_domain)* domain, __itt_scope scope_, __itt_string_handle* key, __itt_metadata_type type, size_t count, void* data);

/** @cond exclude_from_documentation */

alias __itt_metadata_add_with_scope_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_scope scope_, __itt_string_handle* key, __itt_metadata_type type, size_t count, void* data);
extern __gshared __itt_metadata_add_with_scope_ptr__3_0_t __itt_metadata_add_with_scope_ptr__3_0;

extern (D) auto __itt_metadata_add_with_scope(T0, T1, T2, T3, T4, T5)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a, auto ref T5 b)
{
    return ITTNOTIFY_VOID_D5(metadata_add_with_scope, d, x, y, z, a, b);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup parameters
 * @brief Add string metadata to an instance of a named entity.
 * @param[in] domain The domain controlling the call
 * @param[in] scope The scope of the instance to which the metadata is to be added

 * @param[in] id The identifier of the instance to which the metadata is to be added, or __itt_null to add to the current task

 * @param[in] key The name of the metadata
 * @param[in] data The metadata itself
 * @param[in] length The number of characters in the string, or -1 if the length is unknown but the string is null-terminated
*/

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
void __itt_metadata_str_add_with_scope (const(__itt_domain)* domain, __itt_scope scope_, __itt_string_handle* key, const(char)* data, size_t length);

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_metadata_str_add_with_scope_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_scope scope_, __itt_string_handle* key, const(char)* data, size_t length);
extern __gshared __itt_metadata_str_add_with_scope_ptr__3_0_t __itt_metadata_str_add_with_scope_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
extern (D) auto __itt_metadata_str_add_with_scope(T0, T1, T2, T3, T4)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a)
{
    return ITTNOTIFY_VOID_D4(metadata_str_add_with_scope, d, x, y, z, a);
}


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @} metadata group */

/**
 * @defgroup relations Relations
 * Instances of named entities can be explicitly associated with other
 * instances using instance IDs and the relationship API calls.
 *
 * @{
 */

/**
 * @ingroup relations
 * @brief The kind of relation between two instances is specified by the enumerated type __itt_relation.
 * Relations between instances can be added with an API call. The relation
 * API uses instance IDs. Relations can be added before or after the actual
 * instances are created and persist independently of the instances. This
 * is the motivation for having different lifetimes for instance IDs and
 * the actual instances.
 */
enum __itt_relation
{
    __itt_relation_is_unknown = 0,
    __itt_relation_is_dependent_on = 1, /**< "A is dependent on B" means that A cannot start until B completes */
    __itt_relation_is_sibling_of = 2, /**< "A is sibling of B" means that A and B were created as a group */
    __itt_relation_is_parent_of = 3, /**< "A is parent of B" means that A created B */
    __itt_relation_is_continuation_of = 4, /**< "A is continuation of B" means that A assumes the dependencies of B */
    __itt_relation_is_child_of = 5, /**< "A is child of B" means that A was created by B (inverse of is_parent_of) */
    __itt_relation_is_continued_by = 6, /**< "A is continued by B" means that B assumes the dependencies of A (inverse of is_continuation_of) */
    __itt_relation_is_predecessor_to = 7 /**< "A is predecessor to B" means that B cannot start until A completes (inverse of is_dependent_on) */
}

/**
 * @ingroup relations
 * @brief Add a relation to the current task instance.
 * The current task instance is the head of the relation.
 * @param[in] domain The domain controlling this call
 * @param[in] relation The kind of relation
 * @param[in] tail The ID for the tail of the relation
 */
void __itt_relation_add_to_current (const(__itt_domain)* domain, __itt_relation relation, __itt_id tail);

/**
 * @ingroup relations
 * @brief Add a relation between two instance identifiers.
 * @param[in] domain The domain controlling this call
 * @param[in] head The ID for the head of the relation
 * @param[in] relation The kind of relation
 * @param[in] tail The ID for the tail of the relation
 */
void __itt_relation_add (const(__itt_domain)* domain, __itt_id head, __itt_relation relation, __itt_id tail);

/** @cond exclude_from_documentation */

alias __itt_relation_add_to_current_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_relation relation, __itt_id tail);
extern __gshared __itt_relation_add_to_current_ptr__3_0_t __itt_relation_add_to_current_ptr__3_0;
alias __itt_relation_add_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_id head, __itt_relation relation, __itt_id tail);
extern __gshared __itt_relation_add_ptr__3_0_t __itt_relation_add_ptr__3_0;

extern (D) auto __itt_relation_add_to_current(T0, T1, T2)(auto ref T0 d, auto ref T1 x, auto ref T2 y)
{
    return ITTNOTIFY_VOID_D2(relation_add_to_current, d, x, y);
}



extern (D) auto __itt_relation_add(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(relation_add, d, x, y, z);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} relations group */

/** @cond exclude_from_documentation */

struct ___itt_clock_info
{
    ulong clock_freq; /*!< Clock domain frequency */
    ulong clock_base; /*!< Clock domain base timestamp */
}

alias __itt_clock_info = ___itt_clock_info;

/** @endcond */

/** @cond exclude_from_documentation */
alias __itt_get_clock_info_fn = void function (__itt_clock_info* clock_info, void* data);
/** @endcond */

/** @cond exclude_from_documentation */

struct ___itt_clock_domain
{
    __itt_clock_info info; /*!< Most recent clock domain info */
    __itt_get_clock_info_fn fn; /*!< Callback function pointer */
    void* fn_data; /*!< Input argument for the callback function */
    int extra1; /*!< Reserved. Must be zero */
    void* extra2; /*!< Reserved. Must be zero */
    ___itt_clock_domain* next;
}

alias __itt_clock_domain = ___itt_clock_domain;

/** @endcond */

/**
 * @ingroup clockdomains
 * @brief Create a clock domain.
 * Certain applications require the capability to trace their application using
 * a clock domain different than the CPU, for instance the instrumentation of events
 * that occur on a GPU.
 * Because the set of domains is expected to be static over the application's execution time,
 * there is no mechanism to destroy a domain.
 * Any domain can be accessed by any thread in the process, regardless of which thread created
 * the domain. This call is thread-safe.
 * @param[in] fn A pointer to a callback function which retrieves alternative CPU timestamps
 * @param[in] fn_data Argument for a callback function; may be NULL
 */
__itt_clock_domain* __itt_clock_domain_create (__itt_get_clock_info_fn fn, void* fn_data);

/** @cond exclude_from_documentation */

alias __itt_clock_domain_create_ptr__3_0_t = ___itt_clock_domain* function (__itt_get_clock_info_fn fn, void* fn_data);
extern __gshared __itt_clock_domain_create_ptr__3_0_t __itt_clock_domain_create_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup clockdomains
 * @brief Recalculate clock domains frequences and clock base timestamps.
 */
void __itt_clock_domain_reset ();

/** @cond exclude_from_documentation */

alias __itt_clock_domain_reset_ptr__3_0_t = void function ();
extern __gshared __itt_clock_domain_reset_ptr__3_0_t __itt_clock_domain_reset_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup clockdomain
 * @brief Create an instance of identifier. This establishes the beginning of the lifetime of
 * an instance of the given ID in the trace. Once this lifetime starts, the ID can be used to
 * tag named entity instances in calls such as __itt_task_begin, and to specify relationships among
 * identified named entity instances, using the \ref relations APIs.
 * @param[in] domain The domain controlling the execution of this call.
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] id The ID to create.
 */
void __itt_id_create_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id);

/**
 * @ingroup clockdomain
 * @brief Destroy an instance of identifier. This ends the lifetime of the current instance of the
 * given ID value in the trace. Any relationships that are established after this lifetime ends are
 * invalid. This call must be performed before the given ID value can be reused for a different
 * named entity instance.
 * @param[in] domain The domain controlling the execution of this call.
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] id The ID to destroy.
 */
void __itt_id_destroy_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id);

/** @cond exclude_from_documentation */

alias __itt_id_create_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id);
extern __gshared __itt_id_create_ex_ptr__3_0_t __itt_id_create_ex_ptr__3_0;
alias __itt_id_destroy_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id);
extern __gshared __itt_id_destroy_ex_ptr__3_0_t __itt_id_destroy_ex_ptr__3_0;

extern (D) auto __itt_id_create_ex(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(id_create_ex, d, x, y, z);
}



extern (D) auto __itt_id_destroy_ex(T0, T1, T2, T3)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z)
{
    return ITTNOTIFY_VOID_D3(id_destroy_ex, d, x, y, z);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup clockdomain
 * @brief Begin a task instance.
 * @param[in] domain The domain for this task
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] taskid The instance ID for this task instance, or __itt_null
 * @param[in] parentid The parent instance to which this task instance belongs, or __itt_null
 * @param[in] name The name of this task
 */
void __itt_task_begin_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id taskid, __itt_id parentid, __itt_string_handle* name);

/**
 * @ingroup clockdomain
 * @brief Begin a task instance.
 * @param[in] domain The domain for this task
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] taskid The identifier for this task instance, or __itt_null
 * @param[in] parentid The parent of this task, or __itt_null
 * @param[in] fn The pointer to the function you are tracing
 */
void __itt_task_begin_fn_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id taskid, __itt_id parentid, void* fn);

/**
 * @ingroup clockdomain
 * @brief End the current task instance.
 * @param[in] domain The domain for this task
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 */
void __itt_task_end_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp);

/** @cond exclude_from_documentation */

alias __itt_task_begin_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id, __itt_id parentid, __itt_string_handle* name);
extern __gshared __itt_task_begin_ex_ptr__3_0_t __itt_task_begin_ex_ptr__3_0;
alias __itt_task_begin_fn_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id, __itt_id parentid, void* fn);
extern __gshared __itt_task_begin_fn_ex_ptr__3_0_t __itt_task_begin_fn_ex_ptr__3_0;
alias __itt_task_end_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp);
extern __gshared __itt_task_end_ex_ptr__3_0_t __itt_task_end_ex_ptr__3_0;

extern (D) auto __itt_task_begin_ex(T0, T1, T2, T3, T4, T5)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a, auto ref T5 b)
{
    return ITTNOTIFY_VOID_D5(task_begin_ex, d, x, y, z, a, b);
}



extern (D) auto __itt_task_begin_fn_ex(T0, T1, T2, T3, T4, T5)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a, auto ref T5 b)
{
    return ITTNOTIFY_VOID_D5(task_begin_fn_ex, d, x, y, z, a, b);
}



extern (D) auto __itt_task_end_ex(T0, T1, T2)(auto ref T0 d, auto ref T1 x, auto ref T2 y)
{
    return ITTNOTIFY_VOID_D2(task_end_ex, d, x, y);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @defgroup counters Counters
 * @ingroup public
 * Counters are user-defined objects with a monotonically increasing
 * value. Counter values are 64-bit unsigned integers.
 * Counters have names that can be displayed in
 * the tools.
 * @{
 */

/**
 * @brief opaque structure for counter identification
 */
/** @cond exclude_from_documentation */

struct ___itt_counter;
alias __itt_counter = ___itt_counter*;

/**
 * @brief Create an unsigned 64 bits integer counter with given name/domain
 *
 * After __itt_counter_create() is called, __itt_counter_inc(id), __itt_counter_inc_delta(id, delta),
 * __itt_counter_set_value(id, value_ptr) or __itt_counter_set_value_ex(id, clock_domain, timestamp, value_ptr)
 * can be used to change the value of the counter, where value_ptr is a pointer to an unsigned 64 bits integer
 *
 * The call is equal to __itt_counter_create_typed(name, domain, __itt_metadata_u64)
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
__itt_counter __itt_counter_create (const(char)* name, const(char)* domain);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_counter_create_ptr__3_0_t = ___itt_counter* function (const(char)* name, const(char)* domain);
extern __gshared __itt_counter_create_ptr__3_0_t __itt_counter_create_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Increment the unsigned 64 bits integer counter value
 *
 * Calling this function to non-unsigned 64 bits integer counters has no effect
 */
void __itt_counter_inc (__itt_counter id);

alias __itt_counter_inc_ptr__3_0_t = void function (__itt_counter id);
extern __gshared __itt_counter_inc_ptr__3_0_t __itt_counter_inc_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/**
 * @brief Increment the unsigned 64 bits integer counter value with x
 *
 * Calling this function to non-unsigned 64 bits integer counters has no effect
 */
void __itt_counter_inc_delta (__itt_counter id, ulong value);

alias __itt_counter_inc_delta_ptr__3_0_t = void function (__itt_counter id, ulong value);
extern __gshared __itt_counter_inc_delta_ptr__3_0_t __itt_counter_inc_delta_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Decrement the unsigned 64 bits integer counter value
 *
 * Calling this function to non-unsigned 64 bits integer counters has no effect
 */
void __itt_counter_dec (__itt_counter id);

alias __itt_counter_dec_ptr__3_0_t = void function (__itt_counter id);
extern __gshared __itt_counter_dec_ptr__3_0_t __itt_counter_dec_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/**
 * @brief Decrement the unsigned 64 bits integer counter value with x
 *
 * Calling this function to non-unsigned 64 bits integer counters has no effect
 */
void __itt_counter_dec_delta (__itt_counter id, ulong value);

alias __itt_counter_dec_delta_ptr__3_0_t = void function (__itt_counter id, ulong value);
extern __gshared __itt_counter_dec_delta_ptr__3_0_t __itt_counter_dec_delta_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup counters
 * @brief Increment a counter by one.
 * The first call with a given name creates a counter by that name and sets its
 * value to zero. Successive calls increment the counter value.
 * @param[in] domain The domain controlling the call. Counter names are not domain specific.
 *            The domain argument is used only to enable or disable the API calls.
 * @param[in] name The name of the counter
 */
void __itt_counter_inc_v3 (const(__itt_domain)* domain, __itt_string_handle* name);

/**
 * @ingroup counters
 * @brief Increment a counter by the value specified in delta.
 * @param[in] domain The domain controlling the call. Counter names are not domain specific.
 *            The domain argument is used only to enable or disable the API calls.
 * @param[in] name The name of the counter
 * @param[in] delta The amount by which to increment the counter
 */
void __itt_counter_inc_delta_v3 (const(__itt_domain)* domain, __itt_string_handle* name, ulong delta);

alias __itt_counter_inc_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_string_handle* name);
extern __gshared __itt_counter_inc_v3_ptr__3_0_t __itt_counter_inc_v3_ptr__3_0;
alias __itt_counter_inc_delta_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_string_handle* name, ulong delta);
extern __gshared __itt_counter_inc_delta_v3_ptr__3_0_t __itt_counter_inc_delta_v3_ptr__3_0;

extern (D) auto __itt_counter_inc_v3(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(counter_inc_v3, d, x);
}



extern (D) auto __itt_counter_inc_delta_v3(T0, T1, T2)(auto ref T0 d, auto ref T1 x, auto ref T2 y)
{
    return ITTNOTIFY_VOID_D2(counter_inc_delta_v3, d, x, y);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup counters
 * @brief Decrement a counter by one.
 * The first call with a given name creates a counter by that name and sets its
 * value to zero. Successive calls decrement the counter value.
 * @param[in] domain The domain controlling the call. Counter names are not domain specific.
 *            The domain argument is used only to enable or disable the API calls.
 * @param[in] name The name of the counter
 */
void __itt_counter_dec_v3 (const(__itt_domain)* domain, __itt_string_handle* name);

/**
 * @ingroup counters
 * @brief Decrement a counter by the value specified in delta.
 * @param[in] domain The domain controlling the call. Counter names are not domain specific.
 *            The domain argument is used only to enable or disable the API calls.
 * @param[in] name The name of the counter
 * @param[in] delta The amount by which to decrement the counter
 */
void __itt_counter_dec_delta_v3 (const(__itt_domain)* domain, __itt_string_handle* name, ulong delta);

alias __itt_counter_dec_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_string_handle* name);
extern __gshared __itt_counter_dec_v3_ptr__3_0_t __itt_counter_dec_v3_ptr__3_0;
alias __itt_counter_dec_delta_v3_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_string_handle* name, ulong delta);
extern __gshared __itt_counter_dec_delta_v3_ptr__3_0_t __itt_counter_dec_delta_v3_ptr__3_0;

extern (D) auto __itt_counter_dec_v3(T0, T1)(auto ref T0 d, auto ref T1 x)
{
    return ITTNOTIFY_VOID_D1(counter_dec_v3, d, x);
}



extern (D) auto __itt_counter_dec_delta_v3(T0, T1, T2)(auto ref T0 d, auto ref T1 x, auto ref T2 y)
{
    return ITTNOTIFY_VOID_D2(counter_dec_delta_v3, d, x, y);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @} counters group */

/**
 * @brief Set the counter value
 */
void __itt_counter_set_value (__itt_counter id, void* value_ptr);

alias __itt_counter_set_value_ptr__3_0_t = void function (__itt_counter id, void* value_ptr);
extern __gshared __itt_counter_set_value_ptr__3_0_t __itt_counter_set_value_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Set the counter value
 */
void __itt_counter_set_value_ex (__itt_counter id, __itt_clock_domain* clock_domain, ulong timestamp, void* value_ptr);

/** @cond exclude_from_documentation */

alias __itt_counter_set_value_ex_ptr__3_0_t = void function (__itt_counter id, __itt_clock_domain* clock_domain, ulong timestamp, void* value_ptr);
extern __gshared __itt_counter_set_value_ex_ptr__3_0_t __itt_counter_set_value_ex_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Create a typed counter with given name/domain
 *
 * After __itt_counter_create_typed() is called, __itt_counter_inc(id), __itt_counter_inc_delta(id, delta),
 * __itt_counter_set_value(id, value_ptr) or __itt_counter_set_value_ex(id, clock_domain, timestamp, value_ptr)
 * can be used to change the value of the counter
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
__itt_counter __itt_counter_create_typed (const(char)* name, const(char)* domain, __itt_metadata_type type);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_counter_create_typed_ptr__3_0_t = ___itt_counter* function (const(char)* name, const(char)* domain, __itt_metadata_type type);
extern __gshared __itt_counter_create_typed_ptr__3_0_t __itt_counter_create_typed_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Destroy the counter identified by the pointer previously returned by __itt_counter_create() or
 * __itt_counter_create_typed()
 */
void __itt_counter_destroy (__itt_counter id);

alias __itt_counter_destroy_ptr__3_0_t = void function (__itt_counter id);
extern __gshared __itt_counter_destroy_ptr__3_0_t __itt_counter_destroy_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} counters group */

/**
 * @ingroup markers
 * @brief Create a marker instance.
 * @param[in] domain The domain for this marker
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] id The instance ID for this marker, or __itt_null
 * @param[in] name The name for this marker
 * @param[in] scope The scope for this marker
 */
void __itt_marker_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id, __itt_string_handle* name, __itt_scope scope_);

/** @cond exclude_from_documentation */

alias __itt_marker_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id id, __itt_string_handle* name, __itt_scope scope_);
extern __gshared __itt_marker_ex_ptr__3_0_t __itt_marker_ex_ptr__3_0;

extern (D) auto __itt_marker_ex(T0, T1, T2, T3, T4, T5)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a, auto ref T5 b)
{
    return ITTNOTIFY_VOID_D5(marker_ex, d, x, y, z, a, b);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @ingroup clockdomain
 * @brief Add a relation to the current task instance.
 * The current task instance is the head of the relation.
 * @param[in] domain The domain controlling this call
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] relation The kind of relation
 * @param[in] tail The ID for the tail of the relation
 */
void __itt_relation_add_to_current_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_relation relation, __itt_id tail);

/**
 * @ingroup clockdomain
 * @brief Add a relation between two instance identifiers.
 * @param[in] domain The domain controlling this call
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] head The ID for the head of the relation
 * @param[in] relation The kind of relation
 * @param[in] tail The ID for the tail of the relation
 */
void __itt_relation_add_ex (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id head, __itt_relation relation, __itt_id tail);

/** @cond exclude_from_documentation */

alias __itt_relation_add_to_current_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_relation relation, __itt_id tail);
extern __gshared __itt_relation_add_to_current_ex_ptr__3_0_t __itt_relation_add_to_current_ex_ptr__3_0;
alias __itt_relation_add_ex_ptr__3_0_t = void function (const(__itt_domain)* domain, __itt_clock_domain* clock_domain, ulong timestamp, __itt_id head, __itt_relation relation, __itt_id tail);
extern __gshared __itt_relation_add_ex_ptr__3_0_t __itt_relation_add_ex_ptr__3_0;

extern (D) auto __itt_relation_add_to_current_ex(T0, T1, T2, T3, T4)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a)
{
    return ITTNOTIFY_VOID_D4(relation_add_to_current_ex, d, x, y, z, a);
}



extern (D) auto __itt_relation_add_ex(T0, T1, T2, T3, T4, T5)(auto ref T0 d, auto ref T1 x, auto ref T2 y, auto ref T3 z, auto ref T4 a, auto ref T5 b)
{
    return ITTNOTIFY_VOID_D5(relation_add_ex, d, x, y, z, a, b);
}


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @cond exclude_from_documentation */
enum ___itt_track_group_type
{
    __itt_track_group_type_normal = 0
}

alias __itt_track_group_type = ___itt_track_group_type;
/** @endcond */

/** @cond exclude_from_documentation */

struct ___itt_track_group
{
    __itt_string_handle* name; /*!< Name of the track group */
    ___itt_track* track; /*!< List of child tracks    */
    __itt_track_group_type tgtype; /*!< Type of the track group */
    int extra1; /*!< Reserved. Must be zero  */
    void* extra2; /*!< Reserved. Must be zero  */
    ___itt_track_group* next;
}

alias __itt_track_group = ___itt_track_group;

/** @endcond */

/**
 * @brief Placeholder for custom track types. Currently, "normal" custom track
 * is the only available track type.
 */
enum ___itt_track_type
{
    __itt_track_type_normal = 0

    /* INTEL_ITTNOTIFY_API_PRIVATE */
}

alias __itt_track_type = ___itt_track_type;

/** @cond exclude_from_documentation */

struct ___itt_track
{
    __itt_string_handle* name; /*!< Name of the track group */
    __itt_track_group* group; /*!< Parent group to a track */
    __itt_track_type ttype; /*!< Type of the track       */
    int extra1; /*!< Reserved. Must be zero  */
    void* extra2; /*!< Reserved. Must be zero  */
    ___itt_track* next;
}

alias __itt_track = ___itt_track;

/** @endcond */

/**
 * @brief Create logical track group.
 */
__itt_track_group* __itt_track_group_create (__itt_string_handle* name, __itt_track_group_type track_group_type);

/** @cond exclude_from_documentation */

alias __itt_track_group_create_ptr__3_0_t = ___itt_track_group* function (__itt_string_handle* name, __itt_track_group_type track_group_type);
extern __gshared __itt_track_group_create_ptr__3_0_t __itt_track_group_create_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Create logical track.
 */
__itt_track* __itt_track_create (__itt_track_group* track_group, __itt_string_handle* name, __itt_track_type track_type);

/** @cond exclude_from_documentation */

alias __itt_track_create_ptr__3_0_t = ___itt_track* function (__itt_track_group* track_group, __itt_string_handle* name, __itt_track_type track_type);
extern __gshared __itt_track_create_ptr__3_0_t __itt_track_create_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Set the logical track.
 */
void __itt_set_track (__itt_track* track);

/** @cond exclude_from_documentation */

alias __itt_set_track_ptr__3_0_t = void function (__itt_track* track);
extern __gshared __itt_set_track_ptr__3_0_t __itt_set_track_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/* ========================================================================== */
/** @cond exclude_from_gpa_documentation */
/**
 * @defgroup events Events
 * @ingroup public
 * Events group
 * @{
 */
/** @brief user event type */
alias __itt_event = int;

/**
 * @brief Create an event notification
 * @note name or namelen being null/name and namelen not matching, user event feature not enabled
 * @return non-zero event identifier upon success and __itt_err otherwise
 */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
__itt_event __itt_event_create (const(char)* name, int namelen);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_event_create_ptr__3_0_t = int function (const(char)* name, int namelen);
extern __gshared __itt_event_create_ptr__3_0_t __itt_event_create_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an event occurrence.
 * @return __itt_err upon failure (invalid event id/user event feature not enabled)
 */
int __itt_event_start (__itt_event event);

/** @cond exclude_from_documentation */

alias __itt_event_start_ptr__3_0_t = int function (__itt_event event);
extern __gshared __itt_event_start_ptr__3_0_t __itt_event_start_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Record an event end occurrence.
 * @note It is optional if events do not have durations.
 * @return __itt_err upon failure (invalid event id/user event feature not enabled)
 */
int __itt_event_end (__itt_event event);

/** @cond exclude_from_documentation */

alias __itt_event_end_ptr__3_0_t = int function (__itt_event event);
extern __gshared __itt_event_end_ptr__3_0_t __itt_event_end_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} events group */

/**
 * @defgroup arrays Arrays Visualizer
 * @ingroup public
 * Visualize arrays
 * @{
 */

/**
 * @enum __itt_av_data_type
 * @brief Defines types of arrays data (for C/C++ intrinsic types)
 */
enum __itt_av_data_type
{
    __itt_e_first = 0,
    __itt_e_char = 0, /* 1-byte integer */
    __itt_e_uchar = 1, /* 1-byte unsigned integer */
    __itt_e_int16 = 2, /* 2-byte integer */
    __itt_e_uint16 = 3, /* 2-byte unsigned integer  */
    __itt_e_int32 = 4, /* 4-byte integer */
    __itt_e_uint32 = 5, /* 4-byte unsigned integer */
    __itt_e_int64 = 6, /* 8-byte integer */
    __itt_e_uint64 = 7, /* 8-byte unsigned integer */
    __itt_e_float = 8, /* 4-byte floating */
    __itt_e_double = 9, /* 8-byte floating */
    __itt_e_last = __itt_e_double
}

/**
 * @brief Save an array data to a file.
 * Output format is defined by the file extension. The csv and bmp formats are supported (bmp - for 2-dimensional array only).
 * @param[in] data - pointer to the array data
 * @param[in] rank - the rank of the array
 * @param[in] dimensions - pointer to an array of integers, which specifies the array dimensions.
 * The size of dimensions must be equal to the rank
 * @param[in] type - the type of the array, specified as one of the __itt_av_data_type values (for intrinsic types)
 * @param[in] filePath - the file path; the output format is defined by the file extension
 * @param[in] columnOrder - defines how the array is stored in the linear memory.
 * It should be 1 for column-major order (e.g. in FORTRAN) or 0 - for row-major order (e.g. in C).
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
int __itt_av_save (void* data, int rank, const(int)* dimensions, int type, const(char)* filePath, int columnOrder);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_av_save_ptr__3_0_t = int function (void* data, int rank, const(int)* dimensions, int type, const(char)* filePath, int columnOrder);
extern __gshared __itt_av_save_ptr__3_0_t __itt_av_save_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

void __itt_enable_attach ();

/** @cond exclude_from_documentation */

alias __itt_enable_attach_ptr__3_0_t = void function ();
extern __gshared __itt_enable_attach_ptr__3_0_t __itt_enable_attach_ptr__3_0;


/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @cond exclude_from_gpa_documentation */

/** @} arrays group */

/** @endcond */

/**
 * @brief Module load info
 * This API is used to report necessary information in case of module relocation
 * @param[in] start_addr - relocated module start address
 * @param[in] end_addr - relocated module end address
 * @param[in] path - file system path to the module
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */
void __itt_module_load (void* start_addr, void* end_addr, const(char)* path);
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
alias __itt_module_load_ptr__3_0_t = void function (void* start_addr, void* end_addr, const(char)* path);
extern __gshared __itt_module_load_ptr__3_0_t __itt_module_load_ptr__3_0;
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */


/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/* __cplusplus */

/* _ITTNOTIFY_H_ */

/* __cplusplus */

/**
 * @ingroup clockdomain
 * @brief Begin an overlapped task instance.
 * @param[in] domain The domain for this task
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] taskid The identifier for this task instance, *cannot* be __itt_null.
 * @param[in] parentid The parent of this task, or __itt_null.
 * @param[in] name The name of this task.
 */

/**
 * @ingroup clockdomain
 * @brief End an overlapped task instance.
 * @param[in] domain The domain for this task
 * @param[in] clock_domain The clock domain controlling the execution of this call.
 * @param[in] timestamp The user defined timestamp.
 * @param[in] taskid Explicit ID of finished task
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @defgroup makrs_internal Marks
 * @ingroup internal
 * Marks group
 * @warning Internal API:
 *   - It is not shipped to outside of Intel
 *   - It is delivered to internal Intel teams using e-mail or SVN access only
 * @{
 */
/** @brief user mark type */

/**
 * @brief Creates a user mark type with the specified name using char or Unicode string.
 * @param[in] name - name of mark to create
 * @return Returns a handle to the mark type
 */

/* UNICODE */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Creates a "discrete" user mark type of the specified type and an optional parameter using char or Unicode string.
 *
 * - The mark of "discrete" type is placed to collection results in case of success. It appears in overtime view(s) as a special tick sign.
 * - The call is "synchronous" - function returns after mark is actually added to results.
 * - This function is useful, for example, to mark different phases of application
 *   (beginning of the next mark automatically meand end of current region).
 * - Can be used together with "continuous" marks (see below) at the same collection session
 * @param[in] mt - mark, created by __itt_mark_create(const char* name) function
 * @param[in] parameter - string parameter of mark
 * @return Returns zero value in case of success, non-zero value otherwise.
 */

/* UNICODE  */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Use this if necessary to create a "discrete" user event type (mark) for process
 * rather then for one thread
 * @see int __itt_mark(__itt_mark_type mt, const char* parameter);
 */

/* UNICODE  */

/* UNICODE */
/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/** @cond exclude_from_documentation */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */

/* ITT_PLATFORM==ITT_PLATFORM_WIN */
/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Creates an "end" point for "continuous" mark with specified name.
 *
 * - Returns zero value in case of success, non-zero value otherwise.
 *   Also returns non-zero value when preceding "begin" point for the
 *   mark with the same name failed to be created or not created.
 * - The mark of "continuous" type is placed to collection results in
 *   case of success. It appears in overtime view(s) as a special tick
 *   sign (different from "discrete" mark) together with line from
 *   corresponding "begin" mark to "end" mark.
 * @note Continuous marks can overlap and be nested inside each other.
 * Discrete mark can be nested inside marked region
 * @param[in] mt - mark, created by __itt_mark_create(const char* name) function
 * @return Returns zero value in case of success, non-zero value otherwise.
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Use this if necessary to create an "end" point for mark of process
 * @see int __itt_mark_off(__itt_mark_type mt);
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */
/** @} marks group */

/**
 * @defgroup counters_internal Counters
 * @ingroup internal
 * Counters group
 * @{
 */

/**
 * @defgroup stitch Stack Stitching
 * @ingroup internal
 * Stack Stitching group
 * @{
 */
/**
 * @brief opaque structure for counter identification
 */

/**
 * @brief Create the stitch point e.g. a point in call stack where other stacks should be stitched to.
 * The function returns a unique identifier which is used to match the cut points with corresponding stitch points.
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Destroy the inforamtion about stitch point identified by the pointer previously returned by __itt_stack_caller_create()
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief Sets the cut point. Stack from each event which occurs after this call will be cut
 * at the same stack level the function was called and stitched to the corresponding stitch point.
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/**
 * @brief This function eliminates the cut point which was set by latest __itt_stack_callee_enter().
 */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/** @} stitch group */

/* ***************************************************************************************************************************** */

/** @cond exclude_from_documentation */

/*!< no error */
/*!< module can't be loaded */
/* %1$s -- library name; win: %2$d -- system error code; unx: %2$s -- system error message. */
/*!< symbol not found */
/* %1$s -- library name, %2$s -- symbol name. */
/*!< unknown group specified */
/* %1$s -- env var name, %2$s -- group name. */
/*!< GetEnvironmentVariable() failed */
/* %1$s -- env var name, %2$d -- system error. */
/*!< variable value too long */
/* %1$s -- env var name, %2$d -- actual length of the var, %3$d -- max allowed length. */
/*!< pthread_mutexattr_init or pthread_mutex_init failed */
/* %1$s -- function name, %2$d -- errno. */

/** @endcond */

/** @cond exclude_from_documentation */

/* INTEL_NO_ITTNOTIFY_API */

/* INTEL_NO_ITTNOTIFY_API */
/* INTEL_NO_MACRO_BODY */

/* INTEL_NO_MACRO_BODY */
/** @endcond */

/* __cplusplus */

/* _ITTNOTIFY_PRIVATE_ */

/* INTEL_ITTNOTIFY_API_PRIVATE */
