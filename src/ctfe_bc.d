module ctfe_bc; 
import ddmd.expression;
import ddmd.declaration;
import ddmd.dsymbol;
import ddmd.mtype;
import ddmd.declaration : FuncDeclaration;
import ddmd.statement;
import ddmd.visitor;
/**
 * Written By Stefan Koch in 2016
 * All Rights Reserved.
 *? 

/**
	Something about the instruction set :
	The instruction set is designed to be fast to interpret. 
	Therefore it is subject to uncommen restrictions and features uncommen ideoms.
	
	It is one of the few VM instruction sets that do have SIMD-Instructions.
	
	Example include The multiple increment which can increment 2 diffrent stack locations at once.
	(By either 1,4,8, or 16)
	
	The operation-combine marker which allows to execute the same chain of operations on an abitary number of StackLocations.
	
	
*/

import std.conv : to;

/**
 * checks if ThisEnum is contained in ThatEnum
 */
string genToParentEnum(P, E)() {
	assert(__ctfe, "This only makes sense at CTFE");

	string result = " " ~ P.stringof ~ " toParentEnum (const " ~E.stringof ~" e) const pure {
	\tfinal switch(e) with (typeof(e)) {\n";
	import std.format : format;

	foreach(M;__traits(allMembers, E)) {
		result ~= format("\t\tcase %s : return %s.%s;\n", M, P.stringof, M); 
	}
	return result ~ "\n\t}\n}";
}

mixin template mxToParentEnum(P,E) {
	static assert(isIncluded!(E, P));
	mixin(genToParentEnum!(P, E)());
}

template isIncluded(ThisEnum, ThatEnum) {
	import std.traits : EnumMembers, allSatisfy;
	enum emThisEnum = EnumMembers!ThisEnum;
	enum emThatEnum = EnumMembers!ThatEnum;
	static assert(is(ThisEnum == enum) && is(ThatEnum == enum),
		"IsIncluded works only for enums"
	);
//	foreach(m;EnumMembers!ThisEnum) {

//	}

	enum isIncluded =  emThisEnum.length <= emThatEnum.length && 
		hasMembers();

	bool hasMembers() {
		foreach(M;__traits(allMembers, ThisEnum)) {
			static if (!is(typeof(mixin(`ThatEnum.` ~ M)))) {
				static assert(0, "member: " ~ M ~ " is not included");
			}
		}
		return true;
	}
}

struct ByteCode {
//	ubyte[] _data;
}
enum internalPotion = 4096;
struct CtfeStack {
	StackRef[void*/*VariableDeclaration*/] refs;
	ubyte[internalPotion] internalStackMem;

	T getVar(T)(StackRef s) {
		return s.addr < internalPotion ? 
			*(cast(T*)internalStackMem.ptr + s.addr)
			: assert(0, "No external Stack for now"); 
	}
}
struct StackRef {
	short sp;
	Type type;
}
auto evaluateFunction(CallExp ce, ThisExp _this = null) {

//	ce.arguments
}
auto evaluateFunction(FuncDeclaration fd, Expression[] args, ThisExp _this = null) {
	scope BCV bcv = new BCV();
//	bcv.setThis(bcv);
	/*	if (fd.ctfeCode) {
		return executeFun(fd.ctfeCode, args);
	} else {
		fd.ctfeCode = compile(fd);
		return executeFun(fd.ctfeCode, args);
	} */
	if (auto fbody = fd.fbody.isCompoundStatement) {
		foreach(i, p;fd.parameters.opSlice) {
			debug { import std.stdio; 
				writeln("parameter [",i,"] : ", p.toString);
			}
			p.accept(bcv);
		}

		//bcv.visit()
		bcv.visit(fbody);
		debug { import std.stdio;
			bcv.printInstructions.writeln;
			bcv.vars.writeln;
			writeln(" stackUsage = ", (bcv.sp-4).to!string ~ " byte");
		}
	//	foreach(_;0 .. 64) {
		import std.datetime : StopWatch;
		StopWatch sw;
		sw.start;
		writeln("Calling " ~ fd.toString ~" with(100) == ", interpret!int(bcv.byteCodeArray[0 .. bcv.ip], [BCV.BCValue(100, BCV.BCType.i32)]));
		sw.stop;
		writeln("It took " ~ sw.peek.msecs.to!string ~ "msecs");
	//	}
	}
	return null;
}
string toString(T)(T value) if(is(T:Statement) || is(T:Declaration) || is(T:Expression) || is(T:Dsymbol) || is(T:Type)) {
	import core.stdc.string : strlen;
	const (char)* cPtr = value.toChars();
	return cast(string) cPtr[0 .. strlen(cPtr)];
}

T interpret(T) (uint[] byteCode, BCV.BCValue[] args) {
	alias LongInst = BCV.LongInst;
	alias ShortInst = BCV.ShortInst;
	alias InstMask = BCV.InstMask;
	alias InstLengthMask = BCV.InstLengthMask;

	auto stack = new uint[](short.max/4);
	stack.length = short.max/4; // just to make sure;
	uint stackOffset = 4;
	debug { import std.stdio; writeln("Before pushing args"); }

	// first push the args on
	foreach(arg;args) {
		switch (arg.type) {
			case BCV.BCType.i32 : {
				*(cast(uint*)((cast(ubyte*) stack.ptr) + stackOffset)) = arg.imm32;
				stackOffset += uint.sizeof;
			} break;
			default : assert(0, "unsupported Type " ~ to!string(arg.type));
		}
	}

	debug { import std.stdio; writeln("After pushing args"); }

	uint ip = 4;
	bool cond;
	// debug { import std.stdio; writeln("BC.len = ", byteCode.length); } 
	if(byteCode.length == 5)
		return typeof(return).init;

	while(true) {
	
		const lw = byteCode[ip++];  
		if (lw & InstLengthMask) {
			// We have a long instruction
			
			uint hi = byteCode[ip++];
			//	LongInst instValue = );
			switch(cast(LongInst)(lw & InstMask)) {
				case LongInst.ImmAdd : {
					*(cast(uint*)((cast(ubyte*) stack.ptr) + (lw >> 16))) += hi;
					//	writeln("Add SP[" ~ to!string(lw >> 16) ~ "]("~ (*(cast(uint*)((cast(ubyte*) stack.ptr) + (lw >> 16)))).to!string ~ "), #" ~ to!string(hi)); 
				} break;
				case LongInst.ImmSet : {
					*(cast(uint*)((cast(ubyte*) stack.ptr) + (lw >> 16))) = hi;
					//	writeln("Add SP[" ~ to!string(lw >> 16) ~ "]("~ (*(cast(uint*)((cast(ubyte*) stack.ptr) + (lw >> 16)))).to!string ~ "), #" ~ to!string(hi)); 
				} break;
				case LongInst.ImmEq : {
					cond = *(cast(uint*)((cast(ubyte*) stack.ptr) + (lw >> 16))) == hi;
						writeln("Eq SP[" ~ to!string(lw >> 16) ~ "]("~ cond.to!string ~ "), #" ~ to!string(hi)); 
				} break;
				case LongInst.ImmLt : {
					uint lhs = *(cast(uint*)((cast(ubyte*) stack.ptr) + (lw >> 16)));
					uint rhs = hi;
					cond =  lhs < rhs;
					writeln("Lt SP[" ~ to!string(lw >> 16) ~ "]("~ lhs.to!string ~ "), #" ~ to!string(hi)); 
				} break;

				case LongInst.Add : {
					auto lhsOffset = (hi >> 16);
					auto rhsOffset = hi & 0xFFFF;
					uint* lhsRef = (cast(uint*)((cast(ubyte*) stack.ptr) + lhsOffset));
					uint rhs = *(cast(uint*)((cast(ubyte*) stack.ptr) + rhsOffset));
					writeln("Add SP[", lhsOffset, "](", *lhsRef, "), ", "SP[", rhsOffset, "](",rhs,")");
					(*lhsRef) += rhs;
				} break;
				case LongInst.Lt : {
					auto lhsOffset = (hi >> 16);
					auto rhsOffset = hi & 0xFFFF;
					uint lhs = *(cast(uint*)((cast(ubyte*) stack.ptr) + lhsOffset));
					uint rhs = *(cast(uint*)((cast(ubyte*) stack.ptr) + rhsOffset));
					cond = lhs < rhs;
				//	writeln(lhs.to!string, "<" ,rhs.to!string);
				//	writeln("Lt SP[" ~ to!string(rhsOffset) ~ "]("~ (*(cast(uint*)((cast(ubyte*) stack.ptr) + rhsOffset))).to!string ~", SP[" ~ to!string(lhsOffset)  ~ "]"); 
				} break;
				case LongInst.Jmp : {
					ip = hi; 
				//	result ~= "Jmp &" ~ to!string(hi) ~ "\n"; 
				} break;
				case LongInst.TJmp : {
					if (!cond) {
						ip = (hi >> 16);
					}
				//	result ~= "TJmp !SP[" ~ to!string(hi & 0xFFFF) ~ "], &" ~ to!string(hi >> 16)  ~ "\n"; 
				} break;
				default : {
					assert(0, "Unkown LongInst." ~ to!string(cast(LongInst)(lw & InstMask))~" \n");
				} 
			}
		} else {
			// We have a short instruction
			
			switch(cast(ShortInst)(lw & InstMask)) {
				case ShortInst.Ret : {
					auto retval = *(cast(T*)((cast(ubyte*) stack.ptr) + (lw >> 16)));
					writeln("RET: ",retval);
					return retval;

				}

				case ShortInst.Jmp : {
					ip += (cast(short)(lw >> 16)) - 1;
				//	result ~= "Jmp &" ~ to!string(cast(short)(lw >> 16) + pos - 1) ~ "\n"; 
				} break;
				default : {
					assert(0, "Unkown ShortInst \n");
				//	result ~= "Unkown ShortInst \n";
				} 
			}
	}
	}
}


extern(C++) final class BCV : Visitor {
	alias visit = super.visit;

	enum InstLengthMask = ubyte(0x20); // check 6th Byte
	enum InstMask = ubyte(0x1F); // mask for bit 0-5 

	import ddmd.tokens;
	BCValue[void*] vars;

	BCLabel[ubyte.max] unresolvedLabels;
	uint[ushort.max] byteCodeArray;

	// ip starts at 4 because 0 should be an invalid address;
	BCValue retval;
	BCAddr ip = BCAddr(4);
	short sp = 4;

	ubyte unreslovedLabelCount;

	bool discardValue = false;

	/* 
	 * 32bitInst : 
	 * [0 .. 6] Instruction
	 * [6 .. 9] Flags
	 * 
	 * [9 .. 16] (optional InstData)
	 * [16 .. 33] 16BitOperand
	 *
	 *
	 * 64BitInst :
	 * [0 .. 6] Instruction
	 * [6 .. 9] Flags (* Bit 6 is true for 64BitInst
	 * 
	 * 
	 */ 

	string printInstructions() {

		string result = "StartInstructionDump: \n";
		size_t pos = 4;
		import std.conv;
		auto arr = byteCodeArray;
		auto length = ip - 4;
		result ~= "Length : " ~ to!string(length) ~ "\n";


		while (length--) {
			uint lw = arr[pos];
			result ~= pos.to!string ~ ":\t";
			++pos;
			if (lw == 0) {
				result ~= "0x0 0x0 0x0 0x0\n";
				continue;
			}

			if (lw & InstLengthMask) {
				// We have a long instruction

				--length;
				uint hi = arr[pos++];

				switch(cast(LongInst)(lw & InstMask)) {
					case LongInst.ImmSet : {
						result ~= "Set SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n"; 
					} break;
					case LongInst.ImmAdd : {
						result ~= "Add SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n"; 
					} break;
					case LongInst.ImmEq : {
						result ~= "Eq SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n"; 
					} break;
					case LongInst.ImmLt : {
						result ~= "Lt SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n"; 
					} break;
					
					case LongInst.Add : {
						result ~= "Add SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16)  ~ "]\n";
					} break;
					
					case LongInst.Lt : {
						result ~= "Lt SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16)  ~ "]\n"; 
					} break;
					case LongInst.Jmp : {
						result ~= "Jmp &" ~ to!string(hi) ~ "\n"; 
					} break;
					case LongInst.TJmp : {
						result ~= "TJmp !SP[" ~ to!string(hi & 0xFFFF) ~ "], &" ~ to!string(hi >> 16)  ~ "\n"; 
					} break;

					default : {
						result ~= "Unkown LongInst \n" ~ to!string(cast(LongInst)(lw & InstMask));
					} break;
				}
			} else {
				// We have a short instruction

				switch(cast(ShortInst)(lw & InstMask)) {
					case ShortInst.Ret : {
						result ~= "Ret SP[" ~ to!string(lw >> 16) ~ "] \n"; 
					} break;
					case ShortInst.Jmp : {
						result ~= "Jmp &" ~ to!string(cast(short)(lw >> 16) + pos - 1) ~ "\n"; 
					} break;
					default : {
						result ~= "Unkown ShortInst \n";
					} break;
				}
			}
		}
		return result ~ "\n EndInstructionDump";
	} 


	enum ShortInst : ubyte {

		Jmp,
		Ret,

		Add,
		Mul,
		Div,
	}

	static assert(ShortInst.max < 0x20, "Instruction do not fit in 6 byte anymore");

	enum LongInst : ushort {

		Jmp,
		Inc2,

		ImmAdd,
		ImmEq,
		ImmLt,
		ImmGt,
		ImmSet,

		// 2 StackOperands
		Lt,
		TJmp,
		Add,
	}


	/**
	 * Layaout : 
	 * [0-6] Instruction
	 * [6-8] Flags
	 * -----------------
	 * [8-16] Padding
	 * [16-32] StackOffset (lhs)
	 * [32-64] Imm32 (rhs)
	 */ 
	struct LongInstImm32 {
		uint lw;
		uint hi;


		this(LongInstImm32Enum i, short stackAddr, Imm32 imm) pure const {
			mixin mxToParentEnum!(LongInst, LongInstImm32Enum);
			lw = toParentEnum(i) | 1 << 5 | stackAddr << 16;
			hi = imm.imm32; 
		}

		string toString() pure const {
			return to!string(cast(LongInstImm32Enum)(lw & InstMask)) ~ " SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
		}

		enum LongInstImm32Enum {
			// Immidiate operations on one StackValue
			ImmAdd,
			ImmEq,
			ImmLt,
			ImmGt,
			ImmSet,
		}


		alias LongInstImm32Enum this;
	}

	struct LongInst64 {
		uint lw;
		uint hi;

		this(LongInst i, BCAddr addr) {
			lw = i | 1 << 5;
			hi = addr.addr;
		}

		this(LongInst i, short stackAddrLhs, short stackAddrRhs) {
			lw = i | 1 << 5;
			hi = stackAddrLhs | stackAddrRhs << 16;
		}

//		this(LongInst i, ushort imm16, short stackAddrLhs, short stackAddrRhs) {
//			lw = i | 1 << 5 | imm16 << 16;
//			hi = stackAddrLhs | stackAddrRhs << 16;
//		}
	}

	static assert(ShortInst.max < 64);

	uint ShortInst16(ShortInst i, short imm) {
		return i | imm << 16; 
	}

	uint ShortInst24(ShortInst i, uint imm) {
		assert(imm == (imm & 0x0FFF));
		return i | imm << 8; 
	}

	enum BCType {
		undef,

		Void,
		
		Slice,

		String,
		i1,

		i8,
		i16,
		i32,
		i64,
	}

	static const(BCType) toBCType(Type t) /*pure*/ {
		TypeBasic bt = t.isTypeBasic;
		if (bt) {
			switch(bt.ty) {
				case ENUMTY.Tint8 :
				case ENUMTY.Tuns8 :
				case ENUMTY.Tchar :
				return BCType.i8;
				case ENUMTY.Tint16 :
				case ENUMTY.Tuns16 :
					return BCType.i16;
				case ENUMTY.Tint32 :
				case ENUMTY.Tuns32 :
					return BCType.i32;
				case ENUMTY.Tint64 :
				case ENUMTY.Tuns64 :
					return BCType.i64;
				default :
				assert(0, "Type unsupported " ~ (cast(Type)(t)).toString());
			}
		} else {
			if(t.isString) {
				return BCType.String;
			}
			assert(0, "NBT Type unsupported " ~ (cast(Type)(t)).toString());
		}
	}

	struct BCSlice {
		BCType elem;
		uint length;
	}


	enum BCValueType {
		Unknown,
		StackValue,
		Immidiate,
	}

	struct BCValue {
		BCValueType vType;
		short stackAddr;
		BCType type;
		union {
			void* valAddr;

			BCSlice* slice; 

			ubyte* i8;
			ushort* i16;
			uint* i32;
			ulong* i64;

			ulong imm64;
			Imm32 imm32;
		}

		this(Imm32 imm32) pure {
			this.type = BCType.i32;
			this.vType = BCValueType.Immidiate;
			this.imm32 = imm32;
		}

		this(ulong value, BCType type) pure {
			this.type = type;
			this.vType = BCValueType.Immidiate;
			imm64 = value;
		}

		this(StackRef sr) {
			this.stackAddr = sr.sp;
			this.type = toBCType(sr.type);
			this.vType = BCValueType.StackValue;
		}


		this(void* base, short addr, BCType type) pure {
			this.stackAddr = addr;
			this.type = type;
			this.valAddr = base + addr;
			this.vType = BCValueType.StackValue;
		}
	}

//	alias BCValueUnion = BCValue.BCValueUnion;

	struct BCAddr {
		uint addr;
		alias addr this;
	}

	struct BCLabel {
		BCAddr addr;
		ubyte unresolvedId;
	}

	struct BCExpr {
		BCValue value;
		BCBlock evalBlock;
	}

	struct BCBlock {
		BCLabel begin;
		BCLabel end;
	}

	struct Imm32 {
		uint imm32;
		alias imm32 this;
	}

	struct Branch {
		BCValue cond;
		BCLabel ifTrue;
		BCLabel ifFalse;
	}

	enum VisitationType {
		asExpression,
		asDeclatation,
	}

public :

	this() {}
//
//	void integralOp(CTInt a, CTInt b, Tok op) {
//		switch (op) {
//			default : assert(0, "Unhandled op" ~ to!string(op));
//		}
//	}

	BCLabel* unresolvedLabel() {
		unresolvedLabels[unreslovedLabelCount] = BCLabel(BCAddr.init, unreslovedLabelCount);
		return &unresolvedLabel[unreslovedLabelCount++];
	}

	void visit(VarDeclaration v, VisitationType vt) {
		if (vt == VisitationType.asExpression) {
			// bc.emitStackRef(v);
		} else {
			// bc.createStackRef(v);
		}
	}

//	void visit(BinExp be) {
//		if (be.e1.type.isintegral && be.e2.type.isintegral) {
//			intrgralOp(be.e1.ctInt(stack), be.e2.ctInt(stack), be.op);
//		}
//	}

	/*
	 * IMPORTANT make sure the destructor fixes up the jump and stuff :)
	 * genJumpAfterIfFalse(BCExpr* expr, BCBlock* block) {
	 * }
	 */

	static short isShortJump(const int offset) pure {
		assert(offset != 0, "An Jump to the Jump itself is invalid");

		const bool wasNegative = (offset < 0);
		int abs_offset = wasNegative ? offset * -1 : offset;


		if (abs_offset < (1 << 15)) {
			return (cast(ushort)(wasNegative ? abs_offset *= -1 : abs_offset));
		} else {
			return 0;
		}
	}


	BCAddr beginJmp() {
		BCAddr atIp = ip;
		ip += 2;
		return atIp;
	}

	void endJmp(BCAddr atIp, BCLabel target) {
		if (auto offset = isShortJump(target.addr - atIp)) {
			byteCodeArray[atIp] = ShortInst16(ShortInst.Jmp, offset);
		} else {
			LongInst64 lj = LongInst64(LongInst.Jmp, target.addr);
			byteCodeArray[atIp] = lj.lw;
			byteCodeArray[atIp + 1] = lj.hi;
		}
	}

		

	
	BCExpr* genExpr(Expression expr) {

		debug {
			import std.stdio;
		}
		auto ret = new BCExpr();
		ret.evalBlock.begin = genLabel();
		auto oldRetval = retval;
		expr.accept(this);
		ret.value = retval;
		ret.evalBlock.end = genLabel();
		retval = oldRetval;

		return ret;
	}

	BCExpr* genLt(Expression lhs, Expression rhs) {
		auto result = new BCExpr();
		result.evalBlock.begin = genLabel();
		auto _lhs = genExpr(lhs);
		auto _rhs = genExpr(rhs);

		if (_lhs.value.vType == BCValueType.StackValue &&
			_rhs.value.vType == BCValueType.StackValue) {
			emitLongInst(LongInst64(LongInst.Lt, _rhs.value.stackAddr, _lhs.value.stackAddr));
			result.evalBlock.end = genLabel();

			auto resultSp = sp;
			debug {import std.stdio; writeln("resultSp:", resultSp);}
			result.value = BCValue(null, resultSp, BCType.i1);
			retval = result.value;
			sp += align4(1); // sizeof (i1) == 1;
			debug {import std.stdio; writeln("resultSp++:", sp);}

			return result;
		} else if (_lhs.value.vType == BCValueType.StackValue &&
			_rhs.value.vType == BCValueType.Immidiate) {

			emitLongInst(LongInstImm32(LongInstImm32.ImmLt, _lhs.value.stackAddr, _rhs.value.imm32));
			result.evalBlock.end = genLabel();
			
			auto resultSp = sp;
			debug {import std.stdio; writeln("resultSp:", resultSp);}
			result.value = BCValue(null, resultSp, BCType.i1);
			retval = result.value;
			sp += align4(1); // sizeof (i1) == 1;
			debug {import std.stdio; writeln("resultSp++:", sp);}
			
			return result;
		} else {
			assert(0, "only StackValue comparisons are supported at this point");
		}
	}

	BCExpr* genEq(Expression lhs, Expression rhs) {
		auto result = new BCExpr();
		result.evalBlock.begin = genLabel();
		auto _lhs = genExpr(lhs);
		auto _rhs = genExpr(rhs);
		
		if (_lhs.value.vType == BCValueType.StackValue &&
			_rhs.value.vType == BCValueType.Immidiate) {
			emitEq(_lhs.value, _rhs.value);
			result.evalBlock.end = genLabel();
			
			auto resultSp = sp;
			debug {import std.stdio; writeln("resultSp:", resultSp);}
			result.value = BCValue(null, resultSp, BCType.i1);
			retval = result.value;
			sp += align4(1); // sizeof (i1) == 1;
			debug {import std.stdio; writeln("resultSp++:", sp);}
			
			return result;
		} else {
			assert(0, "only StackValue comparisons are supported at this point");
		}
	}

	override void visit(BinExp e) {
		debug {
			import std.stdio;
			writefln("Called visit(BinExp) %s ... \n\tdiscardReturnValue %d", e.toString, discardValue);
			writefln("(BinExp).Op: %s", e.op.to!string);
			//if (auto bt = e.type.isTypeBasic()) {
		//		e.e1.accept(this);
				//assert(isIntegral(bt));
			//} else {
			//	assert(0, "for new we only handle basicTypes :-)");
			//}
		}
		switch (e.op) {
			case TOK.TOKequal : {
				retval = genEq(e.e1, e.e2).value;
			} break;

			default : break ;
		}


	}

	override void visit(IndexExp ie) {
		debug {
			import std.stdio;
			writefln("IndexExpression %s ... \n\tdiscardReturnValue %d", ie.toString, discardValue);
			writefln("ie.type : %s ", ie.type.toString);
		}


	}

	auto genBranch(BCExpr* cond, BCLabel* ifTrue, BCLabel* ifFalse) {
		return null;
	}

	BCBlock* genBlock(Statement stmt) {
		BCBlock* result = new BCBlock();

		result.begin = BCLabel(ip);
		stmt.accept(this);
		result.end = BCLabel(ip);

		return result;
	}

	BCAddr genJump(BCLabel target) {
		assert(target.addr);
		auto at = beginJmp();
		endJmp(at, target);
		debug {
			import std.stdio;
			"Calling genJump".writeln ;
		}
		return at;
	}

	BCLabel genLabel() {
		return BCLabel(ip);
	}

	BCAddr beginTJump() {
		auto at = ip;
		ip += 2;
		return at;
	}

	void endTJump(BCAddr atIp, BCValue cond, BCLabel target) {
		debug { import std.stdio;
			writeln("TJ target : &", target.addr);
			writeln("TJ cond : SP[", cond.stackAddr, "]");
		}
		LongInst64 lj = LongInst64(LongInst.TJmp, cond.stackAddr, cast(short)(target.addr));
		byteCodeArray[atIp] = lj.lw;
		byteCodeArray[atIp + 1] = lj.hi;
		debug { writeln(cast(ushort[]) byteCodeArray[atIp .. atIp+2]);  }

	}

	override void visit(ForStatement fs) {
		debug {
			import std.stdio;
			writefln("ForStatement %s", fs.toString);
		}

		if (fs._init) {
			(fs._init.accept(this));
		}

		if (fs.condition !is null && fs._body !is null) {
			BCLabel beginCond = genLabel();
			BCExpr* cond = genExpr(fs.condition);
			auto tjmp = beginTJump();
			auto _body = genBlock(fs._body);
			if (fs.increment) {
				fs.increment.accept(this);
				_body.end = genLabel();
			}
			genJump(cond.evalBlock.begin);
			auto afterJmp = genLabel();
			endTJump(tjmp, cond.value, afterJmp);
		//	endBranch(branch, cond.value, &cond.evalBlock.begin, &_body.end);
		} else if (fs.condition !is null /* && fs._body is null*/) {
			BCLabel beginCond = genLabel();
			BCExpr* condExpr = genExpr(fs.condition);
			if (fs.increment) {
				fs.increment.accept(this);
			}
		//	genJumpIfTrue(condExpr, &beginCond);
			genJump(beginCond);
		} else { // fs.condition is null && fs._body !is null
			auto _body = genBlock(fs._body);
			if (fs.increment) {
				fs.increment.accept(this);
			}
			genJump(_body.begin);
		}
		
	}

	override void visit(ForeachStatement fs) {
		debug {
			import std.stdio;
			writefln("ForeachStatement %s", fs.toString);
		}
	}

	override void visit(Expression e) {
		debug {
			import std.stdio;
			writefln("Expression %s", e.toString);
		}
	}

	override void visit(VarExp ve) {
		auto vd = cast(void*)ve.var.isVarDeclaration;
		assert(vd, "VarExp " ~ ve.toString ~ "is not a VariableDeclaration !?!");
		auto sv = vd in vars;
		assert(sv, "Variable " ~ ve.toString ~ " not in StackFrame");

		debug {
			import std.stdio;
			writefln("VarExp %s discardValue %d", ve.toString, discardValue);
			writeln("ve.var sp : ", (vd in vars).stackAddr);
		}
		retval = (*sv);
	}

	static const(uint) align4(const uint val) pure {
		return ((val + 3) & ~0b11) ;
	}

	static assert(align4(1) == 4);
	static assert(align4(9) == 12);
	static assert(align4(11) == 12);
	static assert(align4(12) == 12);
	static assert(align4(15) == 16);

	override void visit(DeclarationExp de) {
		auto vd = de.declaration.isVarDeclaration();
		assert(vd, "DeclarationExps are expected to be VariableDeclarations");
		visit(vd);
		debug {
			import std.stdio;
			writefln("DeclarationExp %s discardValue %d", de.toString, discardValue);
			writefln("DeclarationExp.declaration: %x", cast(void*)de.declaration.isVarDeclaration);
		}
		auto var = BCValue(StackRef(cast(short)(sp - align4(cast(uint)vd.type.size)), vd.type));

		if (auto ci = vd.getConstInitializer) {
			auto oldRetVal = retval;
			ci.accept(this);
			if(retval.vType == BCValueType.Immidiate && retval.type == BCType.i32) {
				//TODO SetStackValue Instruction
				emitSet(var, retval);
			}
			retval = oldRetVal;
		}
		assert(sp < ushort.max, "StackOverflow Stack is currently constrained to 64K");

		//de.declaration.accept(this);
	
	}

	override void visit(VarDeclaration vd) {
		debug {
			import std.stdio;
			writefln("VariableDeclaration %s discardValue %d", vd.toString, discardValue);
		}
		auto var = BCValue(StackRef(sp, vd.type));
		vars[cast(void*)vd] = var;
		sp += cast(short) align4(cast(uint)vd.type.size);
		debug { import std.stdio; writeln("StackPointer after push: ", sp); }

		writeln(vd.type.size);
	}

	override void visit(BinAssignExp e) {
		debug {
			import std.stdio;
			writefln("BinAssignExp %s discardValue %d", e.toString, discardValue);
		}
		auto oldRetval = retval;
		e.e1.accept(this);
		auto lhs = retval;
		//assert(lhs.vType == BCValueType.StackValue);
		e.e2.accept(this);
		auto rhs = retval;
		//assert(rhs.vType == BCValueType.Immidiate);

		assert(rhs.type == BCType.i32 && lhs.type == BCType.i32); 

		switch (e.op) {

			case TOK.TOKaddass : {
				emitAdd(lhs, rhs);  
			}
			break;
			default : {
				assert(0, "Unsupported for now");
			}
		}
		//assert(discardValue);

		retval = oldRetval;
	}

	void emitLongInst(LongInst64 i) {
		byteCodeArray[ip] = i.lw;
		byteCodeArray[ip + 1] = i.hi;
		ip += 2;
	}

	void emitLongInst(LongInstImm32 i) {
		byteCodeArray[ip] = i.lw;
		byteCodeArray[ip + 1] = i.hi;
		ip += 2;
	}



	void emitEq(BCValue lhs, BCValue rhs) {
		assert(lhs.vType == BCValueType.StackValue);
		assert(rhs.vType == BCValueType.Immidiate);
		assert(rhs.type == BCType.i32 && lhs.type == BCType.i32);
		
		debug {
			import std.stdio;
			writeln("emitEq("~ ip.to!string ~ "|" ~  lhs.to!string ~ ", " ~ rhs.to!string ~ ")");
		}
		
		emitLongInst(LongInstImm32(LongInstImm32.ImmEq, lhs.stackAddr, rhs.imm32));
	}

	void emitSet(BCValue lhs, BCValue rhs) {
		assert(rhs.type == BCType.i32 && lhs.type == BCType.i32, "for now only 32bit set is supported");
		if (lhs.vType == BCValueType.StackValue &&
			rhs.vType == BCValueType.Immidiate) {
			
			emitLongInst(LongInstImm32(LongInstImm32.ImmSet, lhs.stackAddr, rhs.imm32));
		} /*else if (lhs.vType == BCValueType.StackValue &&
			rhs.vType == BCValueType.StackValue) {
			
			emitLongInst(LongInst64(LongInst.Set, lhs.stackAddr, rhs.stackAddr));
		}*/ else {
			assert(0, "Set flavour unsupported");
		}
	}

	void emitAdd(BCValue lhs, BCValue rhs) {
		assert(rhs.type == BCType.i32 && lhs.type == BCType.i32);
		if (lhs.vType == BCValueType.StackValue &&
			rhs.vType == BCValueType.Immidiate) {

			emitLongInst(LongInstImm32(LongInstImm32.ImmAdd, lhs.stackAddr, rhs.imm32));
		} else if (lhs.vType == BCValueType.StackValue &&
			rhs.vType == BCValueType.StackValue) {

			emitLongInst(LongInst64(LongInst.Add, lhs.stackAddr, rhs.stackAddr));
		} else {
			assert(0, "Add flavour unsupported");
		}
	}

	void emitReturn(BCValue val) {
		assert(val.vType == BCValueType.StackValue);
		byteCodeArray[ip] = ShortInst.Ret | val.stackAddr << 16;
		ip += 1;
	}

	override void visit(IntegerExp ie) {
		debug {
			import std.stdio;
			writefln("IntegerExpression %s", ie.toString);
		}

		auto bct = toBCType(ie.type);
		assert(bct == BCType.i32);
		retval = BCValue(ie.getInteger(), bct);
		assert(retval.vType == BCValueType.Immidiate);
	}

	override void visit(CmpExp ce) {
		debug {
			import std.stdio;
			writefln("CmpExp %s discardValue %d", ce.toString, discardValue);
		}


		 switch(ce.op) {
			case TOK.TOKlt : {
				retval = genLt(ce.e1, ce.e2).value;
			} break;
			
			case TOK.TOKge : {
				retval = genLt(ce.e2, ce.e1).value;
			} break;

			default : assert(0, "Unsupported Operation " ~to!string(ce.op));
		}
	}


	override void visit(AssignExp ae) {
	 	debug {
			import std.stdio;
			writefln("AssignExp %s", ae.toString);

			ae.e1.toString().writeln;
			ae.e1.toString().writeln;
			ae.e2.accept(this);
		}

	}

	override void visit(SwitchStatement ss) {
		debug {
			import std.stdio;
			writefln("SwitchStatement %s", ss.toString);
			writefln("SwitchStatement._body %s", ss._body.toString);
			writefln("SwitchStatement.condition %s type :%s", ss.condition.toString, ss.condition.type.toString);
		}

		auto cond = genExpr(ss.condition);
	}

	override void visit(ReturnStatement rs) {
		debug {
			import std.stdio;
			writefln("ReturnStatement %s", rs.toString);
		}

		rs.exp.accept(this);
		emitReturn(retval);
	}

	override void visit(CastExp ce) {
		// just go over the cast as if it were not there :)
		ce.e1.accept(this);
	}

	override void visit(ExpStatement es) {
		debug {
			import std.stdio;
			writefln("ExpStatement %s", es.toString);
		}
		immutable oldDiscardValue = discardValue;
		discardValue = true;
		genExpr(es.exp);
		discardValue = oldDiscardValue;
	}

	override void visit(Statement s) {
		debug {
			import std.stdio;
			writefln("Statement %s", s.toString);
		}

		//s.accept(this);
	}

	override void visit(ScopeStatement ss) {
		debug {
			import std.stdio;
			writefln("ScopeStatement %s", ss.toString);
		}
		const oldsp = sp;
		ss.statement.accept(this);
		sp = oldsp;
	}


//	override void visit(Assign) {
//		super.visit();
//	}

	override void visit(CompoundStatement cs) {
		debug {
			import std.stdio;
			writefln("CompundStatement %s", cs.toString);
		}

//		BCV thisbcv = new BCV();
		foreach(stmt; cs.statements.opSlice()) {
//			auto stmt = (*cs.statements)[i];
/*			if(auto _cs = stmt.isCompoundStatement()) {visit(_cs);}
			else if (auto es = stmt.isExpStatement()) {visit(es);}
			else if (auto rs = stmt.isReturnStatement()) {visit(rs);}
			else if (auto fs = stmt.isForStatement()) {visit(fs);}
			else */{stmt.accept(this);}
		}
	}
}
