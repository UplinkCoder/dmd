module ddmd.typefunction;

import ddmd.expression;
import ddmd.declaration;
import ddmd.mtype;
import ddmd.func;
import ddmd.visitor;

extern (C++) final class TypeFunctionHandler : Visitor
{
    alias visit = super.visit;

    override void visit(FuncDeclaration fd) {

    }

    bool isValidTypeFunctionType (TypeFunction tf)
    {

    }

}

