module ctfe_bc; 
import ddmd.expression;
import ddmd.declaration;
import ddmd.dsymbol;
import ddmd.mtype;
import ddmd.declaration : FuncDeclaration;
import ddmd.statement;
import ddmd.visitor;

struct ByteCode {
//	ubyte[] _data;
}
enum internalPotion = 4096;
struct CtfeStack {
	StackRef[size_t/*VariableDeclaration*/] refs;
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
auto evaluateFunction(FuncDeclaration fd, Expression[] args, ThisExp _this = null) {
	BCV bcv = new BCV();
//	bcv.setThis(bcv);
	/*	if (fd.ctfeCode) {
		return executeFun(fd.ctfeCode, args);
	} else {
		fd.ctfeCode = compile(fd);
		return executeFun(fd.ctfeCode, args);
	} */
//	foreach(a;fd.v_arguments)
	if (auto fbody = fd.fbody.isCompoundStatement) {
		bcv.visit(fbody);
		debug { import std.stdio;
			bcv.printInstructions.writeln;
			writeln("stackUsage = ", bcv.sp-4);
		}
	}
	return null;
}
string toString(T)(T value) if(is(T:Statement) || is(T:Declaration) || is(T:Expression) || is(T:Dsymbol) || is(T:Type)) {
	import core.stdc.string : strlen;
	const (char)* cPtr = value.toChars();
	return cast(string) cPtr[0 .. strlen(cPtr)];
}


extern(C++) final class BCV : Visitor {
	alias visit = super.visit;

	import ddmd.tokens;
	import std.conv : to;
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

		enum InstLengthMask = ubyte(0x20); // check 6th Byte
		enum InstMask = ubyte(0x1F); // mask for bit 0-5 

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
			//	LongInst instValue = );  
				switch(cast(LongInst)(lw & InstMask)) {
					case LongInst.ImmAdd : {
						result ~= "Add SP[" ~ to!string(lw >> 16) ~ "], " ~ to!string(hi) ~ "\n"; 
					} break;
					case LongInst.Jmp : {
						result ~= "Jmp &" ~ to!string(hi) ~ "\n"; 
					} break;
					default : {
						result ~= "Unkown LongInst \n";
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

	}

	struct LongInst64 {
		uint lw;
		uint hi;

		this(LongInst i, uint imm) {
			lw = i | 1 << 5;
			hi = imm;
		}

		this(LongInst i, short stackAddr, uint imm) {
			lw = i | 1 << 5 | stackAddr << 16;
			hi = imm; 
		}
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

		i8,
		i16,
		i32,
		i64,
	}

	static const(BCType) toBCType(Type t) /*pure*/ {
		switch(t.isTypeBasic.ty) {
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
			uint imm32;
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
	//	BCLabel beforeExp = genLabel();

		debug {
			import std.stdio;

			expr.accept(this);
		}

		return null;
	}

	override void visit(BinExp e) {
		debug {
			import std.stdio;
			writefln("Called visit(BinExp) %s ... \n\tdiscardReturnValue %d", e.toString, discardValue);
			//if (auto bt = e.type.isTypeBasic()) {
				e.e1.accept(this);
				//assert(isIntegral(bt));
			//} else {
			//	assert(0, "for new we only handle basicTypes :-)");
			//}
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

	BCAddr beginBranch() {
		auto at = ip;
		ip += 4;
		return at;
	}

	void endBranch(BCAddr atIp, BCValue cond, BCLabel* ifTrue, BCLabel* ifFalse) {
	//	assert(cond !is null);

	}

	void genJumpIfFalse(BCExpr* expr, BCLabel* label) {
		auto branchAt = genBranch(expr, null, label);
	}

	auto genJumpIfTrue(BCExpr* expr, BCLabel* label) {
		auto branchAt = genBranch(expr, label, null);
	}

	void genReturn(Expression expr) {

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
			BCLabel afterBody;
			auto branch = beginBranch();
			auto _body = genBlock(fs._body);
			if (fs.increment) {
				fs.increment.accept(this);
				_body.end = genLabel();
			}
			genJump(beginCond);
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

	override void visit(Expression e) {
		debug {
			import std.stdio;
			writefln("Expression %s", e.toString);
		}
	}

	override void visit(VarExp ve) {
		auto vd = cast(void*)ve.var.isVarDeclaration;
		assert(vd, "VarExp " ~ ve.toString ~ "is not a VariableDeclaration !?!");

		debug {
			import std.stdio;
			writefln("VarExp %s discardValue %d", ve.toString, discardValue);
			writeln("ve.var sp : ", (vd in vars).stackAddr);
		}
		auto sv = vd in vars;
		assert(sv, "Variable " ~ ve.toString ~ " not in StackFrame");
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

		debug {
			import std.stdio;
			writefln("DeclarationExp %s discardValue %d", de.toString, discardValue);
			writefln("DeclarationExp.declaration: %x", cast(void*)de.declaration.isVarDeclaration);
		}
		auto var = BCValue(StackRef(sp, vd.type));

		if (auto ci = vd.getConstInitializer) {
			auto oldRetVal = retval;
			ci.accept(this);
			if(retval.vType == BCValueType.Immidiate && retval.type == BCType.i32) {
				//TODO SetStackValue Instruction
				//emitSet(var, retval);
			}
			retval = oldRetVal;
		}
		vars[cast(void*)vd] = var;
		sp += align4(cast(uint)vd.type.size);

		assert(sp < ushort.max, "StackOverflow Stack is currently constrained to 64K");
		writeln(vd.type.size);

		//de.declaration.accept(this);
	
	}

	override void visit(VarDeclaration vd) {
		debug {
			import std.stdio;
			writefln("VariableDeclaration %s discardValue %d", vd.toString, discardValue);
		}
	}

	override void visit(BinAssignExp e) {
		debug {
			import std.stdio;
			writefln("BinAssignExp %s discardValue %d", e.toString, discardValue);
		}
		auto oldRetval = retval;
		e.e1.accept(this);
		auto lhs = retval;
		assert(lhs.vType == BCValueType.StackValue);
		e.e2.accept(this);
		auto rhs = retval;
		assert(rhs.vType == BCValueType.Immidiate);

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
		assert(discardValue);

		retval = oldRetval;
	}

	void emitAdd(BCValue lhs, BCValue rhs) {
		assert(lhs.vType == BCValueType.StackValue);
		assert(rhs.vType == BCValueType.Immidiate);
		assert(rhs.type == BCType.i32 && lhs.type == BCType.i32);

		debug {
			import std.stdio;
			writeln("emitAdd("~ ip.to!string ~ "|" ~  lhs.to!string ~ ", " ~ rhs.to!string ~ ")");
		}
		auto i = LongInst64(LongInst.ImmAdd, lhs.stackAddr, rhs.imm32);
		byteCodeArray[ip] = i.lw;
		byteCodeArray[ip + 1] = i.hi;
		ip += 2;
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
	}


	override void visit(AssignExp ae) {
	 	debug {
			import std.stdio;
			writefln("AssignExp %s", ae.toString);

			ae.e1.toString().writeln;
			ae.e2.toString().writeln;
		}

	}

	override void visit(ReturnStatement rs) {
		debug {
			import std.stdio;
			writefln("ReturnStatement %s", rs.toString);
		}

		rs.exp.accept(this);
		emitReturn(retval);
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
