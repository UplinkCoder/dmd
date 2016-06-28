module ctfe_bc; 
import ddmd.expression;
import ddmd.declaration;
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
struct StackRef {}
auto evaluateFunction(FuncDeclaration fd, StackRef[] args, ThisExp _this = null) {
	BCV bcv;
	/*	if (fd.ctfeCode) {
		return executeFun(fd.ctfeCode, args);
	} else {
		fd.ctfeCode = compile(fd);
		return executeFun(fd.ctfeCode, args);
	} */

	if (auto fbody = fd.fbody.isCompoundStatement) {
		bcv.visit(fbody);


	}
	return null;
}
string toString(T)(T value) if(is(T:Statement) || is(T:Declaration) || is(T:Expression)) {
	import core.stdc.string : strlen;
	const (char)* cPtr = value.toChars();
	return cast(string) cPtr[0 .. strlen(cPtr)];
}
extern(C++) final class BCV : Visitor {
	alias visit = super.visit;

	import ddmd.tokens;
	import std.conv : to;
	uint[ushort.max] byteCodeArray;
	BCAddr ip = BCAddr(4);

	enum ShortInst : ubyte {
		Jmp,
		Add,
		Mul,
		Div,
	}

	enum LongInst : ushort {
		Jmp,
		Add,

	}

	struct LongInst64 {
		uint lw;
		uint hi;

		this(LongInst i, uint imm) {
			lw = 0b10 | i << 2 ;
			hi = imm; 
		}
	}

	static assert(ShortInst.max < 64);

	void emit(T)(T bcE) {
		static if (is(T == Jmp*)) {

		} else static assert(0, T.stringof ~ " not supported");
	}


	uint ShortInst16(ShortInst i, short imm) {
		return 0b00 | i << 2 | imm << 16; 
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
	
	struct BCSlice {
		BCType elem;
		uint length;
	}

	struct BCValue {
		uint stackAddr;
		BCType type;
		union {
			void* valAddr;

			BCSlice* slice; 

			ubyte* i8;
			ushort* i16;
			uint* i32;
			ulong* i64;
		}

		

		this(void* base, uint addr, BCType type) pure {
			this.stackAddr = addr;
			this.type = type;
			this.valAddr = base + addr;
		}
	}

//	alias BCValueUnion = BCValue.BCValueUnion;

	struct BCAddr {
		uint addr;
		alias addr this;
	}

	struct BCLabel {
		BCAddr addr;
	}

	struct BCExpr {
		BCValue value;
	}

	struct BCBlock {
		BCLabel begin;
		BCLabel end;
	}

	enum VisitationType {
		asExpression,
		asDeclatation,
	}

public :
//
//	void integralOp(CTInt a, CTInt b, Tok op) {
//		switch (op) {
//			default : assert(0, "Unhandled op" ~ to!string(op));
//		}
//	}

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
			writeln(cast(BinExp)expr !is null);
			writeln(cast(AssignExp)expr !is null);
	//		visit(expr);
		}

		return null;
	}

	override void visit(BinExp e) {
		debug {
			import std.stdio;
			writeln("Called visit(BinExp)");
		}
	}

	auto genBranch(BCExpr* cond, BCLabel* ifTrue, BCLabel* ifFalse) {
		return null;
	}

	BCBlock* genBlock(Statement stmt) {
		BCBlock* result = new BCBlock();

		result.begin = BCLabel(ip);
		visit(stmt);
		result.end = BCLabel(ip);

		return result;
	}

	BCAddr genJump(BCLabel target) {
		assert(target.addr);
		auto at = beginJmp();
		endJmp(at, target);
		return at;
	}

	BCLabel genLabel() {
		return BCLabel(ip);
	}

	BCAddr beginCondJump(BCExpr* cond) {
		auto at = ip;
		ip += 2;
		return at;
	}

	auto endCondJump(BCLabel* ifTrue, BCLabel* ifFalse) {

	}

	void genJumpIfFalse(BCExpr* expr, BCLabel* label) {
		auto branchAt = genBranch(expr, null, label);
	}

	auto genJumpIfTrue(BCExpr* expr, BCLabel* label) {
		auto branchAt = genBranch(expr, label, null);
	}

	override void visit(ForStatement fs) {
		debug {
			import std.stdio;
			writefln("ForStatement %s", fs.toString);
		}

		if (fs._init) {
			visit(fs._init);
		}

		if (fs.condition !is null && fs._body !is null) {
			BCExpr* cond = genExpr(fs.condition);
			auto cjmp = beginCondJump(cond);
			auto _body = genBlock(fs._body);
			if (fs.increment) {
				visit(fs.increment);
				_body.end = genLabel();
			}
			endCondJump(null, &_body.end);
		} else if (fs.condition !is null /* && fs._body is null*/) {
			BCLabel beginCond = genLabel();
			BCExpr* condExpr = genExpr(fs.condition);
			if (fs.increment) {
				visit(fs.increment);
			}
			genJumpIfTrue(condExpr, &beginCond);
		} else { // fs.condition is null && fs._body !is null
			auto _body = genBlock(fs._body);
			if (fs.increment) {
				visit(fs.increment);
			}
			genJump(_body.begin);
		}

	}

	override void visit(ReturnStatement rs) {
		debug {
			import std.stdio;
			writefln("ReturnStatement %s", rs.toString);
		}
		
	}


	override void visit(ExpStatement es) {
		debug {
			import std.stdio;
			writefln("ExpStatement %s", es.toString);
		}

		genExpr(es.exp);
	}

/*	override void visit(Statement s) {
		debug {
			import std.stdio;
			writefln("Statement %s", s.toString);
		}

	}
*/
	override void visit(CompoundStatement cs) {
		debug {
			import std.stdio;
			writefln("CompundStatement %s", cs.toString);
		}
		foreach(stmt; cs.statements.opSlice()) {
//			auto stmt = (*cs.statements)[i];
			if(auto _cs = stmt.isCompoundStatement()) {visit(_cs);}
			else if (auto es = stmt.isExpStatement()) {visit(es);}
			else if (auto rs = stmt.isReturnStatement()) {visit(rs);}
			else if (auto fs = stmt.isForStatement()) {visit(fs);}
			else {stmt.accept(this);}
		}
	}


}
