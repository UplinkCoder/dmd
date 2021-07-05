/// This module contains ABI definitions in form of structs and helper functions

module ddmd.ctfe.bc_abi;

import ddmd.ctfe.bc_limits;

/// appended to a struct
/// behind the last member
struct StructMetaData
{
    enum VoidInitBitfieldOffset = 0;
    enum Size = 4;
}

/// appended to union
/// behind the biggest Member
struct UnionMetaData
{
    enum VoidInitBitfieldOffset = 0;
    enum Size = bc_max_members/8;
}

/// prepended to a class
/// before the first member
struct ClassMetaData
{
    enum VtblOffset = 0;
    enum TypeIdIdxOffset = 4;
    enum Size = 8;
}

/// SliceDescriptor is the ABI for a { ptr, size } aggregate
/// known as a slice
struct SliceDescriptor
{
    enum BaseOffset = 0;
    enum LengthOffset = 4;
    enum CapcityOffset = 8;
    enum ExtraFlagsOffset = 12;
    enum Size = 16;
}

/// DelegateDescriptor is the ABI for a { funcPtr, ContextPtr } aggregate
/// known as a delegate
struct DelegateDescriptor
{
    enum FuncPtrOffset = 0;
    enum ContextPtrOffset = 4;
    enum Size = 8;
}

