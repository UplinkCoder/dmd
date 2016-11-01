module ddmd.ctfe.bc_macro;

import ddmd.ctfe.bc_common;
//TODO the macro could be improved to only do 255 and at the end.
//leading to less code-bloat
/*
 * 
 */ 
void Byte3Macro(BCGen)(BCGen* gen, BCValue _result, BCValue word, BCValue idx)
{
    with(gen) {
    Eq3(BCValue.init, idx, BCValue(Imm32(0)));
    auto cndJmp1 = beginCndJmp();
    auto tmp1 = genTemporary(BCType(BCTypeEnum.i32));//SP[12]
    And3(_result, word, BCValue(Imm32(255)));
    auto label3 = genLabel();
    endCndJmp(cndJmp1, label3);
    Eq3(BCValue.init, idx, BCValue(Imm32(1)));
    auto cndJmp2 = beginCndJmp();
    And3(tmp1, word, BCValue(Imm32(65280)));
    Rsh3(_result, tmp1, BCValue(Imm32(8)));
    auto label5 = genLabel();
    endCndJmp(cndJmp2, label5);
    Eq3(BCValue.init, idx, BCValue(Imm32(2)));
    auto cndJmp3 = beginCndJmp();
    And3(tmp1, word, BCValue(Imm32(16711680)));
    Rsh3(_result, tmp1, BCValue(Imm32(16)));
    auto label7 = genLabel();
    endCndJmp(cndJmp3, label7);
    Eq3(BCValue.init, idx, BCValue(Imm32(3)));
    auto cndJmp4 = beginCndJmp();
    And3(tmp1, word, BCValue(Imm32(4278190080)));
    Rsh3(_result, tmp1, BCValue(Imm32(24)));
    auto label9 = genLabel();
    endCndJmp(cndJmp4, label9);
    //Assert(Error);
    }
}


void StringCat3Macro(BCGen)(BCGen* gen, BCValue result, BCValue lhs, BCValue rhs)
{
	with (gen) {
		auto lhsLength = genTemporary(BCType(BCTypeEnum.i32));
		auto rhsLength = genTemporary(BCType(BCTypeEnum.i32));
		auto combinedLength = genTemporary(BCType(BCTypeEnum.i32));

		Load32(lhsLength, lhs.i32);
		Load32(rhsLength, rhs.i32);
		Add3(combinedLength, lhsLength, rhsLength);

		// don't forget 4 byte for the length :)
		Add3(combinedLength, combinedLength, bcFour);

		Alloc(result, combinedLength);
		Store32(result, combinedLength);
		auto position = genTemporary(i32Type);
		Add3(position, result, bcFour);


		auto resultPos = genTemporary(BCValue(BCTypeEnum.i32));


	}
}

//TODO This macro currenly computes the ByteOffset and Index two times
//Wereas one time should suffice 
// Delete the duplicated code!
// Also remove unnecessary jumps at the end
/*
bool strEq(string a, string b, bool result = false;)
{
    result = false;

    if (a.length == b.length)
    {
        result = true;
        int length = cast(int)a.length;
        while(length--)
        {
            if (a[length] != b[length])
            {
                result = false;
                break;
            }
        }
    }

    return result;
}

 */ 

void StringEq3Macro(BCGen)(BCGen* gen, BCValue _result, BCValue lhs, BCValue rhs)
{
    with(gen) {
		auto p1 = lhs.i32;//SP[4]
		auto p2 = rhs.i32;//SP[8]
		auto p3 = genTemporary(BCType(BCTypeEnum.i32));//SP[12]
		auto p4 = _result;//SP[16]
		Set(p4, BCValue(Imm32(0)));
		auto tmp1 = genTemporary(BCType(BCTypeEnum.i32));//SP[20]
		auto tmp2 = genTemporary(BCType(BCTypeEnum.i32));//SP[24]
		Load32(tmp2, p1);
		auto tmp3 = genTemporary(BCType(BCTypeEnum.i32));//SP[28]
		Load32(tmp3, p2);
		Eq3(tmp1, tmp2, tmp3);
		auto cndJmp1 = beginCndJmp(tmp1);
		auto label1 = genLabel();
		Set(p4, BCValue(Imm32(1)));
		Load32(p3, p1);
		auto label2 = genLabel();
		auto tmp4 = genTemporary(BCType(BCTypeEnum.i32));//SP[32]
		Set(tmp4, p3);
		Sub3(p3, p3, BCValue(Imm32(1)));
		auto cndJmp2 = beginCndJmp(tmp4);
		auto label3 = genLabel();
		auto tmp5 = genTemporary(BCType(BCTypeEnum.i32));//SP[36]
		auto tmp6 = genTemporary(BCType(BCTypeEnum.i32));//SP[40]
		Add3(tmp6, p1, BCValue(Imm32(1)));
		auto tmp7 = genTemporary(BCType(BCTypeEnum.Char));//SP[44]
		auto tmp8 = genTemporary(BCType(BCTypeEnum.i32));//SP[48]
		auto tmp9 = genTemporary(BCType(BCTypeEnum.i32));//SP[52]
		Mod3(tmp9, p3, BCValue(Imm32(4)));
		Div3(tmp8, p3, BCValue(Imm32(4)));
		Add3(tmp6, tmp6, tmp8);
		Load32(tmp7, tmp6);
		Byte3(tmp7, tmp7, tmp9);
		auto tmp10 = genTemporary(BCType(BCTypeEnum.i32));//SP[56]
		Add3(tmp10, p2, BCValue(Imm32(1)));
		auto tmp11 = genTemporary(BCType(BCTypeEnum.Char));//SP[60]
		auto tmp12 = genTemporary(BCType(BCTypeEnum.i32));//SP[64]
		auto tmp13 = genTemporary(BCType(BCTypeEnum.i32));//SP[68]
		Mod3(tmp13, p3, BCValue(Imm32(4)));
		Div3(tmp12, p3, BCValue(Imm32(4)));
		Add3(tmp10, tmp10, tmp12);
		Load32(tmp11, tmp10);
		Byte3(tmp11, tmp11, tmp13);
		Neq3(tmp5, tmp7, tmp11);
		auto cndJmp3 = beginCndJmp(tmp5);
		auto label4 = genLabel();
		Set(p4, BCValue(Imm32(0)));
		auto jmp1 = beginJmp();
		auto label5 = genLabel();
		endJmp(jmp1, label5);
		auto jmp2 = beginJmp();
		auto label6 = genLabel();
		//auto label6 = genLabel();
		endJmp(jmp2, label6);
		endCndJmp(cndJmp3, label6);
		auto label7 = genLabel();
		genJump(label2);
		auto label8 = genLabel();
		endCndJmp(cndJmp2, label8);
		auto label9 = genLabel();

		endCndJmp(cndJmp1, label9);
	}
}

/+
void StringEq3Macro(BCGen)(BCGen* gen, BCValue _result, BCValue lhs, BCValue rhs)
{
     with(gen) {
        auto lhsLength = genTemporary(i32Type);
        auto rhsLength = genTemporary(i32Type);
        Load32(lhsLength, lhs.i32);
        Load32(rhsLength, rhs.i32);
        Eq3(_result, lhsLength, rhsLength);
        auto r  = genTemporary(i32Type);
        Set(r, _result);
        auto length_equals_jmp = beginCndJmp(_result);

        auto lhsPtr = genTemporary(i32Type);
        auto rhsPtr = genTemporary(i32Type);
        Add3(lhsPtr, lhs.i32, bcOne);
        Add3(rhsPtr, rhs.i32, bcOne);
        // The prevoius add jump over the length
        {
            auto Lcompare_loop = genLabel();

            Eq3(BCValue.init, lhsLength, bcZero);
            auto endLoopJmp = beginCndJmp();
            Sub3(lhsLength, lhsLength, bcOne);
            BCValue lhsElem = genTemporary(i32Type);
            BCValue rhsElem = genTemporary(i32Type);
            Load32(lhsElem, lhsPtr); /* this translates to result = *lhs++ == *rhs++ */
            Load32(rhsElem, rhsPtr);
            Add3(lhsPtr, lhsPtr, bcOne);
            Add3(rhsPtr, rhsPtr, bcOne);
            Eq3(_result, lhsElem, rhsElem);
            auto jmp_to_cmpr = beginCndJmp(_result, false);
            auto Lafter_cmp_loop = genLabel();
            endCndJmp(endLoopJmp, Lafter_cmp_loop);
            endCndJmp(jmp_to_cmpr, Lcompare_loop);
        }
        endCndJmp(length_equals_jmp, genLabel());
    }
}
+/
