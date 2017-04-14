/*===-- llvm-c/Support.h - Support C Interface --------------------*- C -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This file defines the C interface to the LLVM support library.             *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

module llvm.c.support;

import llvm.c.core;

extern(C) nothrow:

/**
 * @defgroup LLVMCSupportTypes Types and Enumerations
 *
 * @{
 */

alias LLVMBool = int;

/**
 * Used to pass regions of memory through LLVM interfaces.
 *
 * @see llvm::MemoryBuffer
 */
struct __LLVMOpaqueMemoryBuffer {};
alias LLVMMemoryBufferRef = __LLVMOpaqueMemoryBuffer*;

/**
 * @}
 */

/**
 * This function permanently loads the dynamic library at the given path.
 * It is safe to call this function multiple times for the same library.
 *
 * @see sys::DynamicLibrary::LoadLibraryPermanently()
  */
LLVMBool LLVMLoadLibraryPermanently(const(char)* Filename);

/**
 * This function parses the given arguments using the LLVM command line parser.
 * Note that the only stable thing about this function is its signature; you
 * cannot rely on any particular set of command line arguments being interpreted
 * the same way across LLVM versions.
 *
 * @see llvm::cl::ParseCommandLineOptions()
 */
void LLVMParseCommandLineOptions(int argc, const(char*)* argv,
                                 const(char)* Overview);
