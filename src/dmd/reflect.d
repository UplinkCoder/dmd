/// this module implements builtins for core.reflect.

module dmd.reflect;
import dmd.func;

import dmd.errors;
import dmd.tokens;
import dmd.id;
import dmd.expression;
import dmd.arraytypes;
import dmd.dinterpret;
import dmd.globals;
import dmd.astcodegen;
import dmd.astenums;
import core.stdc.stdio;

enum emitFunctionBodies = false;

enum REFLECT
{
    Invalid,

    currentScope,
    declarationsFromTokenString,
    nodeFromName,
}

bool isInReflectionPackage(FuncDeclaration func)
{
    auto fd = func.toAliasFunc();
    if (fd.isDeprecated())
        return REFLECT.Invalid;
    auto m = fd.getModule();
    if (!m || !m.md)
        return REFLECT.Invalid;
    const md = m.md;
    return md.packages.length >= 2
        && md.packages[0] == Id.core
        && md.packages[1] == Id.reflect;
}

bool isReflect(FuncDeclaration fd)
{
    return isInReflectionPackage(fd)
        || hasCoreReflectAnnotation(fd);
}

bool hasCoreReflectAnnotation(Declaration d)
{
    bool result = false;
    if (d.userAttribDecl is null || d.userAttribDecl.atts is null)
        return result;

    foreach(attr;*d.userAttribDecl.getAttributes())
    {
        if (TupleExp te = attr.isTupleExp())
        {
            auto len = te.exps.length;
            auto e0 = len ? (*te.exps)[0] : null;
            if (len == 1)
            {
                ScopeExp sexp = e0.isScopeExp();
                Package pkg = sexp && sexp.sds ? sexp.sds.isPackage() : null;
                bool isReflectPackage = pkg ? pkg.ident == Id.reflect && pkg.parent && pkg.parent.ident == Id.core : false;
                if (isReflectPackage)
                {
                    result = true;
                    break;
                }
            }
        }
    }

    return result;
}

REFLECT reflectKind(FuncDeclaration func)
{
    REFLECT kind;
    auto fd = func.toAliasFunc();
    if (fd.isDeprecated())
        return REFLECT.Invalid;
    auto m = fd.getModule();
    if (!m || !m.md)
        return REFLECT.Invalid;
    const md = m.md;
    // Look for core.reflect.<package>

    if (!md.packages.length)
        return REFLECT.Invalid;

    const id1 = md.packages[0];
    if (id1 != Id.core)
        return REFLECT.Invalid;

    const id2 = (md.packages.length >= 2) ? md.packages[1] : md.id;
    //const id3 = (md.packages.length >= 3) ? md.packages[2] : md.id; // ignored for now
    const id4 = fd.ident;

    if (id2 == Id.reflect)
    {
        if (id4 == Id.declarationsFromTokenString) return REFLECT.declarationsFromTokenString;
        else if (id4 == Id.nodeFromName) return REFLECT.nodeFromName;
        else if (id4 == Id.currentScope) return REFLECT.currentScope;
    }

    return REFLECT.Invalid;
}
import dmd.dscope;
static bool isScope(Expression e)
{
    auto cre = e.isClassReferenceExp();
    return cre && cre.type.equivalent(ReflectionVisitor.getCd("Scope").type);
}

Expression eval_reflect(const ref Loc loc, REFLECT reflect_kind, Expressions* args, Scope* lookupScope)
{
    final switch(reflect_kind)
    {
        case REFLECT.Invalid :
            return null;
        case REFLECT.declarationsFromTokenString :
        {
            initialize();
            import dmd.parse;

            ClassDeclaration cd = ReflectionVisitor.getCd("Declaration");

            assert((*args).length == 1);
            Expression args0 = (*args)[0];
            import core.stdc.stdio;
            auto se = args0.isStringExp();
            assert(se);
            auto code = se.peekString();
            scope p = new Parser!ASTCodegen(loc, null, code, true);
            // printf("string: %s\n", code.ptr);
            p.nextToken();
            auto mod = p.parseModule();
            import dmd.dmodule;
            import dmd.dsymbolsem;

            if (mod.length == 1)
            {
                //                dsymbolSemantic((*mod)[0], sc.push());
                mod = expandDecls((*mod)[0]);
            }
            auto expressions = new Expressions(mod.length);
            foreach(i, d;*mod)
            {
                if (auto fd = d.isFuncDeclaration())
                {
                    (*expressions)[i] = makeReflectionClassLiteral(fd, lookupScope);
                }
                else if (auto vd = d.isVarDeclaration())
                {
                    (*expressions)[i] = makeReflectionClassLiteral(vd, lookupScope);
                }
                else if (auto ed = d.isEnumDeclaration())
                {
                    (*expressions)[i] = makeReflectionClassLiteral(ed, lookupScope);
                }
                else
                {
                    import dmd.asttypename;
                    printf("d: %s (%s) \n", d.toChars(), astTypeName(d).ptr);
                    printf("creating delcaration is unsupported\n");
                    // (*expressions)[i] = makeReflectionClassLiteral(d, lookupScope);
                    (*expressions)[i] = new NullExp(d.loc, cd.type);
                }
/+
                else if (auto imp = d.isImport())
                {
                    printf("found an import\n");
                }
+/                
            }
            auto result = new ArrayLiteralExp(loc, cd.type.arrayOf, expressions);

            // printf("declDefs: %s\n", mod.toChars());
            // parse()
            return result;
        }
        case REFLECT.currentScope :
        {
            initialize();
            ClassReferenceExp result;
            result = makeReflectionClassLiteral(lookupScope);
            return result;
        }
        case REFLECT.nodeFromName :
        {
            initialize();
            Expression result;
            assert(args.length == 2, "nodeFromName needs 2 arguments exactly");
            Expression name_string = (*args)[0];
            Expression lookupScopeExp = (*args)[1];
            assert(isScope(lookupScopeExp), "Second argument of nodeFromName has to be Scope");
            auto se = name_string.isStringExp();
            assert(se);
            auto name = se.peekString();

            auto scope_value = (*lookupScopeExp.isClassReferenceExp().value.elements)[0];
            assert(scope_value);
            auto lsie = scope_value.isIntegerExp();
            assert(lsie);
            auto mylookupScope = cast(Scope*) lsie.getInteger();

            Dsymbol out_scope;
            auto sym_id = Identifier.idPool(name);
            auto sym = mylookupScope.search(loc, sym_id, &out_scope);
            //printf("sym_id: %s\n", sym_id.toChars());
            if (sym)
            {
                result = makeReflectionClassLiteral(sym, mylookupScope);
            }
            else
            {
                result = new NullExp(loc);
            }
            return result;
        }
    }
}

import dmd.visitor;
import dmd.ctfeexpr;
import dmd.dmodule;
import dmd.dimport;
import dmd.identifier;
import dmd.dsymbolsem;

/// loads core.reflect from druntime and builds tables
/// so we can make sure the runtime functions and classes are in sync
/// with this code.
void initialize()
{
    __gshared Import impCoreReflect = null;
    __gshared Identifier[2] coreReflectID;
    if (!impCoreReflect)
    {
        //printf("Intializing tables for core.reflect\n");
        coreReflectID[0] = Id.core;
        coreReflectID[1] = Id.reflect;
        auto s = new Import(Loc.initial, coreReflectID[], Id.reflect, null, false);
        // Module.load will call fatal() if there's no core.reflect.reflect available.
        // Gag the error here, pushing the error handling to the caller.
        uint errors = global.startGagging();
        s.load(null);
        if (s.mod)
        {
            s.mod.importAll(null);
            s.mod.dsymbolSemantic(null);
        }
        global.endGagging(errors);
        impCoreReflect = s;

        if (s.mod.members) foreach(sym;*s.mod.members)
        {
            scope expandVisitorX = new ExpandDeclVisitor();
            sym.accept(expandVisitorX);
            core_reflect.append(expandVisitorX.syms);
        }
        //        printf("\ncore.reflect syms: %s\n", core_reflect.toChars());
        fillReflectClasses();
        fillReflectEnums();
    }

}
import dmd.denum;
__gshared ASTCodegen.ClassReferenceExp[void*] types;
__gshared ASTCodegen.Dsymbols* core_reflect = new ASTCodegen.Dsymbols;
__gshared ASTCodegen.ClassDeclaration[] core_reflect_classes;
__gshared ASTCodegen.EnumDeclaration[] core_reflect_enums;

/// Gather all classes in core.reflect.*
private void fillReflectClasses()
{
    foreach(m;*core_reflect)
    {
        if (auto cd = m.isClassDeclaration())
        {
            cd.dsymbolSemantic(null);
            core_reflect_classes ~= cd;
            import core.stdc.stdio;
            // printf("cd: %s\n", cd.toChars());
        }
    }
}

/// Gather all enums in core.reflect.*
private void fillReflectEnums()
{
    foreach(m;*core_reflect)
    {
        if (auto ed = m.isEnumDeclaration())
        {
            ed.dsymbolSemantic(null);
            core_reflect_enums ~= ed;
            import core.stdc.stdio;
            //printf("ed: %s\n", ed.toChars());
        }
    }
}


ASTCodegen.Dsymbols* expandDecls(ASTCodegen.Dsymbol s)
{
    scope expandVisitor = new ExpandDeclVisitor();
    s.accept(expandVisitor);
    return expandVisitor.syms;
}
import dmd.dclass;
import dmd.declaration;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.mtype;
import dmd.ast_node;

StringExp makeString(const char* s, Loc loc = Loc.initial)
{
    import core.stdc.string : strlen;
    auto len = (s ? strlen(s) : 0);
    StringExp result = new StringExp(loc, s[0 .. len]);
    result.type = Type.tstring;
    return result;
}


private ClassReferenceExp makeReflectionClassLiteral(Scope* sc, Loc loc = Loc.initial)
{
    assert(sc !is null, "Scope must not be null when creating reflection class for scopes");
    const ivalue = cast(size_t)(cast(void*)sc);
    auto cd = ReflectionVisitor.getCd("Scope");
    ClassReferenceExp result;
    Expressions* elements = new Expressions(1);
    assert((cd.fields)[0].toString() == "internalPointer", (cd.fields)[0].toString());
    (*elements)[0] = new IntegerExp(loc, ivalue, Type.tvoidptr);
    auto data = new StructLiteralExp(loc, cast(StructDeclaration)cd, elements);
    result = new ClassReferenceExp(loc, data, cd.type.immutableOf());

    return result;
}

ClassReferenceExp makeReflectionClassLiteral(ASTNode n, Scope* sc)
{
    if (auto cre = (cast(void*) n) in ReflectionVisitor.cache)
    {
        return *cast(ClassReferenceExp*)cre;
    }
/+
    import dmd.asttypename;
    printf("%s: '%s' {astTypeName: %s}}\n",
        __FUNCTION__.ptr, n.toChars(), n.astTypeName().ptr);
+/
    scope reflectionVisitor = new ReflectionVisitor(n, sc);
    n.accept(reflectionVisitor);


    auto result = reflectionVisitor.result;

    ReflectionVisitor.cache[cast(void*)n] = result;

    return result;
}

extern(C++) final class ReflectionVisitor : SemanticTimeTransitiveVisitor
{
    __gshared ClassReferenceExp[void*] cache;

    alias visit = SemanticTimeTransitiveVisitor.visit;

    ASTNode node;
    ClassDeclaration cd;
    Expressions* elements;
    StructLiteralExp data;
    ClassReferenceExp result;
    Scope* lookupScope;
    Loc loc;
    bool leaf = true;

    this(ASTNode n, Scope* sc = null, Loc loc = Loc.initial)
    {
        elements = new Expressions();
        this.lookupScope = sc;
        this.loc = loc;
        this.node = n;
    }

    extern(D) static ClassDeclaration getCd(string s)
    {
        ClassDeclaration cd;
        foreach(c;core_reflect_classes)
        {
            if (c.toString == s)
            {
                cd = c;
                break;
            }
        }
        assert(cd, "Reflection class '" ~ s ~ "' could not be found");
        return cd;
    }

    extern(D) static EnumDeclaration getEd(string s)
    {
        EnumDeclaration ed;
        foreach(e;core_reflect_enums)
        {
            if (e.toString == s)
            {
                ed = e;
                break;
            }
        }
        assert(ed, "Reflection enum '" ~ s ~ "' could not be found");
        return ed;
    }

    static private bool matches(Type expType, Type targetType)
    {
        bool typeIsCorrect = expType.equivalent(targetType);
        // if the types are not equivalent it still may be the case that the expression is a subtype
        // of the field it's put in. let's look for base-classes and compare
        if (!typeIsCorrect)
        {
            if (expType.implicitConvTo(targetType) != MATCH.nomatch)
            {
                typeIsCorrect = true;
            }
            auto ctype = expType.isTypeClass();
            if (ctype)
            {
                auto cd = ctype.sym;
                while(cd.baseClass)
                {
                    cd = cd.baseClass;
                    if (cd.type.implicitConvTo(targetType))
                    {
                        typeIsCorrect = true;
                        break;
                    }
                }
            }
        }
        return typeIsCorrect;
    }

    ClassReferenceExp placeholder(ClassDeclaration pcd = null)
    {
        auto ncd = pcd ? pcd : cd;
        auto placeholder_data = new StructLiteralExp(loc, cast(StructDeclaration)ncd, elements);
        return new ClassReferenceExp(loc, placeholder_data, ncd.type.immutableOf());
    }

    void finalize()
    {
        enum debug_core_reflect = 0;
        static if (debug_core_reflect)
        {
            uint currentField = 0;
            auto currentElement = elements.length - cd.fields.length;
            auto test = cd;
            while(test.baseClass)
            {
                foreach(field;test.fields)
                {
                    auto expType = (*elements)[currentElement++].type;
                    auto fieldType = field.type;
                    assert(matches(expType, fieldType),
                       test.toString ~ " seems to mismatch at '" ~ field.toString ~ "' expType: " ~ expType.toString() ~ " fieldType: " ~ fieldType.toString());
                }
                currentElement -= test.fields.length;
                test = test.baseClass;
                currentElement -= test.fields.length;
            }
        }
        data = new StructLiteralExp(loc, cast(StructDeclaration)cd, elements);
        if (auto placeholder = (cast(void*)node) in cache)
        {
            placeholder.value = data;
            result = *placeholder;
        }
        else
        {
            result = new ClassReferenceExp(loc, data, cd.type.immutableOf());
        }
    }

    override void visit(ASTCodegen.TypeEnum t)
    {
        assert(leaf);
        leaf = 0;
        visit(cast(Type)t);
        leaf = 1;
        cd = getCd("TypeEnum");

        auto enumDecl = makeReflectionClassLiteral(t.sym, lookupScope);
        fillField((cd.fields)[0], "sym", elements, enumDecl);

        if (leaf)
            finalize();
    }

    override void visit(StringExp e)
    {
        assert(leaf);
        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Expression)e);
        leaf = oldLeaf;
        cd = getCd("StringLiteral");
        fillField((cd.fields)[0], "string_", elements, e);
        if (leaf)
            finalize();
    }

    override void visit(IntegerExp e)
    {
        assert(leaf);
        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Expression)e);
        leaf = oldLeaf;
        cd = getCd("IntegerLiteral");
        auto oldType = e.type;
        e.type = Type.tuns64;
        fillField((cd.fields)[0], "value", elements, e);
        e.type = oldType;
        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.TypePointer t)
    {
        cd = getCd("TypePointer");
        visit(cast(TypeNext)t);
    }

    override void visit(ASTCodegen.TypeSArray t)
    {
        cd = getCd("TypeArray");
        visit(cast(TypeNext)t);
        fillField((cd.fields)[0], "dim", elements, t.dim);
    }

    override void visit(ASTCodegen.TypeDArray t)
    {
        cd = getCd("TypeSlice");
        visit(cast(TypeNext)t);
    }


    override void visit(ASTCodegen.TypeNext t)
    {
        // assert that we came from direct child of of type next;
        auto base = cd.baseClass;
        assert(base.toString() == "TypeNext", base.toString());

        auto oldLeaf = leaf;
        import dmd.typesem;
        auto resolved = typeSemantic(t.next, loc, lookupScope);
        if (resolved.ty != TY.Terror)
        {
            t.next = resolved;
        }

        leaf = 0;
        visit(cast(Type)t);
        leaf = oldLeaf;

        auto tnext = makeReflectionClassLiteral(resolved, lookupScope);
        fillField((base.fields)[0], "nextOf", elements, tnext);

        if (leaf)
            finalize();
    }

    override void visit(TypeIdentifier ti)
    {
        import dmd.typesem;
        // type identifiers are unresolved. try to resolve them!
        auto t = typeSemantic(ti, loc, lookupScope);
        visit(t);
    }

    override void visit(TypeTypeof tt)
    {
        import dmd.typesem;
        // type typeofs are unresolved. try to resolve them!
        auto t = typeSemantic(tt, loc, lookupScope);
        visit(t);
    }


    override void visit(ASTCodegen.Type t)
    {
        auto oldCd = cd;
        cd = getCd("Type");
        t = t.merge2();
        auto p = placeholder(oldCd ? oldCd : cd);
        cache[cast(void*)t] = p;


        uint size;
        uint alignSize;

        size = t.isTypeFunction() ? 0 : cast(uint)t.size();
        alignSize = t.isTypeFunction() ? 0 : t.alignsize();

        elements.setDim(4);
        auto kind = makeString(t.kind());//tyToTypeKind(cast(ENUMTY)t.ty);
        fillField((cd.fields)[0], "kind", elements, kind, 0);

        auto alignSizeExp = new IntegerExp(loc, alignSize, Type.tuns32);
        fillField((cd.fields)[1], "alignSize", elements, alignSizeExp, 1);

        auto sizeExp = new IntegerExp(loc, size, Type.tuns64);
        fillField((cd.fields)[2], "size", elements, sizeExp, 2);

        auto identifier = makeString(t.toChars(), loc);
        fillField((cd.fields)[3], "identifier", elements, identifier, 3);


        if (leaf)
            finalize();
        else
            cd = oldCd;
    }

    private extern (D) void fillField(VarDeclaration vd, string fieldName, Expressions* elements, Expression exp, int expIndex = -1,
        string file = __FILE__, uint line = __LINE__)
    {
        import dmd.utils;

        string loc = "+" ~ itos(line) ~ " " ~ file;
        assert(vd.toString() == fieldName,
            "[" ~ loc ~ "] " ~ "Expected to fill field: '" ~ fieldName ~ "' but got '" ~ vd.toString()  ~ "'"
        );

        assert(exp, "[" ~ loc ~ "] " ~ "Null Expression passed");

        assert(matches(exp.type, vd.type),
            "[" ~ loc ~ "] " ~ "mismatched types between field '" ~ vd.type.toString() ~ "'"
            ~ " and expression '" ~ exp.type.toString ~ "'");

        if (expIndex >= 0)
        {
            (*elements)[expIndex] = exp;
        }
        else
        {
            elements.push = exp;
        }
    }

    override void visit(ASTCodegen.Parameter p)
    {
        assert(leaf);
        cd = getCd("FunctionParameter");
        elements.setDim(2);

        fillField((cd.fields)[0], "type", elements, makeReflectionClassLiteral(p.type, lookupScope), 0);

        Expression identifier;
        if (p.ident)
        {
            identifier = makeString(p.ident.toChars(), loc);
        }
        else
        {
            identifier = new NullExp(loc, Type.tstring);
        }

        fillField((cd.fields)[1], "identifier", elements, identifier, 1);

        finalize();
    }

    override void visit(ASTCodegen.Declaration d)
    {

        auto oldCd = cd;
        cd = getCd("Declaration");

        auto p = placeholder(oldCd ? oldCd : cd);
        cache[cast(void*)d] = p;

        fillField((cd.fields)[0], "name", elements, makeString(d.ident.toChars(), loc));

        auto e0 = new Expressions(0);
        fillField((cd.fields)[1], "attributes", elements,
            new ArrayLiteralExp(loc, getCd("Node").type.arrayOf, e0)
        );

        fillField((cd.fields)[2], "linkage", elements,
            LINKtoLinkage(d.linkage)
        );

        fillField((cd.fields)[3], "comment", elements, makeString(d.comment, loc));

        if (leaf)
            finalize();
        else
            cd = oldCd;
    }

    override void visit (ASTCodegen.TypeFunction ft)
    {
        assert(leaf);

        cd = getCd("FunctionType");
        auto base = cd.baseClass;

        leaf = 0;
        visit(cast(Type)ft);
        leaf = 1;

        auto returnType = makeReflectionClassLiteral(ft.next, lookupScope);
        fillField((cd.fields)[0], "returnType", elements, returnType);

        auto nParams = ft.parameterList.length;

        Expressions* parameterTypeElements = new Expressions(nParams);
        auto cParameter = getCd("FunctionParameter");
        foreach(i, p;ft.parameterList)
        {
            auto para = makeReflectionClassLiteral(p, lookupScope);
            assert(matches(para.type, cParameter.type));
            (*parameterTypeElements)[i] = para;
        }

        auto parameterTypeArray = new ArrayLiteralExp(loc,
            cParameter.type.arrayOf,
            parameterTypeElements,
        );
        fillField((cd.fields)[1], "parameterTypes", elements, parameterTypeArray);

        finalize();
    }

    static Expression LINKtoLinkage(LINK linkage)
    {
        EnumDeclaration ed = getEd("Linkage");
        string lookFor;

        final switch(linkage)
        {
            case LINK.default_ :
                lookFor = "Default";
            break;
            case LINK.d :
                lookFor = "D";
            break;
            case LINK.c :
                lookFor = "C";
            break;
            case LINK.cpp :
                lookFor = "CPP";
            break;
            case LINK.windows :
                lookFor = "Windows";
            break;
            case LINK.objc :
                lookFor = "ObjC";
            break;
            case LINK.system :
                lookFor = "System";
        }

        foreach(i, m;*ed.members)
        {
            EnumMember em = m.isEnumMember();
            assert(em);

            if (em.toString() == lookFor)
            {
                return (em.value);
            }
        }

        assert(0, "Linkage." ~ lookFor ~ " could not be found in LinkageEnum");

    }

    override void visit(ASTCodegen.Initializer i)
    {

    }

    override void visit(ASTCodegen.ExpInitializer i)
    {
        import dmd.asttypename;
        (i.exp).accept(this);

    }

    override void visit(ASTCodegen.EnumDeclaration ed)
    {
        cd = getCd("EnumDeclaration");

        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Declaration)ed);
        leaf = oldLeaf;

        auto type = makeReflectionClassLiteral(ed.type, lookupScope);
        fillField(cd.fields[0], "type", elements, type);

        auto memberCd = getCd("EnumMember");

        auto nMembers = ed.members.length;
        Expressions* enumMembers = new Expressions(nMembers);

        if (nMembers) foreach(i, m; *ed.members)
        {
            auto em = m.isEnumMember();
            (*enumMembers)[i] = makeReflectionClassLiteral(em, lookupScope);
        }
        auto members
            = new ArrayLiteralExp(loc, memberCd.type.arrayOf(), enumMembers);

        fillField(cd.fields[1], "members", elements, members);

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.EnumMember m)
    {
        cd = getCd("EnumMember");

        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Declaration)m);
        leaf = oldLeaf;

        auto value = makeReflectionClassLiteral(m.value(), lookupScope);
        fillField((cd.fields)[0], "value", elements, value);

        if (leaf)
            finalize();
    }


    override void visit(ASTCodegen.IdentifierExp e)
    {
        // (cast(Expression)e) = expressionSemantic(e, lookupScope);
        assert(0, "Unresolved expression detected");
    }


    override void visit(Expression e)
    {
        assert(!leaf);
        auto oldCd = cd;
        cd = getCd("Expression");

        if(!e.type)
        {
            assert(0, "Expression '" ~ e.toString ~ "' is supposed to have a type");
        }
        auto type = makeReflectionClassLiteral(e.type, lookupScope);
        assert(type, e.toString());
        fillField((cd.fields)[0], "type", elements, type);

        if (!leaf)
            cd = oldCd;
    }

    override void visit (ASTCodegen.VarDeclaration vd)
    {
        cd = getCd("VariableDeclaration");

        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Declaration)vd);
        leaf = oldLeaf;

        assert(vd.type);
        auto type = makeReflectionClassLiteral(vd.type, lookupScope);
        fillField((cd.fields)[0], "type", elements, type);


        auto _init = vd.init ? makeReflectionClassLiteral(vd._init, lookupScope) : new NullExp(loc, getCd("Expression").type);
        fillField((cd.fields)[1], "_init", elements, _init);

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.CompoundStatement ce)
    {
        cd = getCd("BlockStatement");

        auto nStatements = ce.statements ? ce.statements.length : 0;
        Expressions* statementElems = new Expressions(nStatements);
        if (nStatements) foreach(i, s; *ce.statements)
        {
            (*statementElems)[i] = makeReflectionClassLiteral(s, lookupScope);
        }
        auto statements =
            new ArrayLiteralExp(loc, getCd("Statement").type.arrayOf, statementElems);

        fillField(cd.fields[0], "statements", elements, statements);

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.ReturnStatement s)
    {
        assert(leaf);

        cd = getCd("ReturnStatement");
        auto exp = s.exp ? makeReflectionClassLiteral(s.exp, lookupScope) : new NullExp(loc, getCd("Expression").type);
        fillField(cd.fields[0], "exp", elements, exp);

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.VarExp e)
    {
        assert(leaf);
        cd = getCd("VariableExpression");

        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Expression)e);
        leaf = oldLeaf;

        auto var = makeReflectionClassLiteral(e.var, lookupScope);
        fillField((cd.fields)[0], "var", elements, var);

        if (leaf)
            finalize();
    }


    override void visit(ASTCodegen.FuncDeclaration fd)
    {
        cd = getCd("FunctionDeclaration");

        auto oldLeaf = leaf;
        leaf = 0;
        visit(cast(Declaration)fd);
        leaf = oldLeaf;

        bool semaDone = fd.functionSemantic();
        bool sema3Done = fd.functionSemantic3();

        auto type = makeReflectionClassLiteral(cast(TypeFunction)fd.type, lookupScope);
        fillField((cd.fields)[0], "type", elements, type);

        // second element is core.reflect.decl.VariableDeclaration[] parameters;
        assert((cd.fields)[1].toString() == "parameters", (cd.fields)[1].toString());
        const nParameters = fd.parameters ? fd.parameters.length : 0;

        Expressions* parameterElements = new Expressions(nParameters);
        if (nParameters) foreach(i, p; *fd.parameters)
        {
            (*parameterElements)[i] = makeReflectionClassLiteral(p, lookupScope);
        }
        auto parameterArrray = new ArrayLiteralExp(loc, (cd.fields[1]).type, parameterElements);

        fillField((cd.fields)[1], "parameters", elements, parameterArrray);

        auto oldlookupScope = lookupScope;
        lookupScope = fd._scope;
        // third element is core.reflect.stmt.Statement fbody
        // TODO FIXME: serialize the body statement into here
        auto fbody = (fd.fbody && emitFunctionBodies) ? makeReflectionClassLiteral(fd.fbody, lookupScope)
            : new NullExp(loc, getCd("Statement").type);
        fillField((cd.fields)[2], "fbody", elements, fbody);
        lookupScope = oldlookupScope;

        if (leaf)
            finalize();
    }
}

extern(C++) final class ExpandDeclVisitor : SemanticTimeTransitiveVisitor
{
    alias visit = SemanticTimeTransitiveVisitor.visit;

    ASTCodegen.Dsymbols* syms;
    LINK currentLinkage;
    StorageClass currentStorageClass;
    Visibility currentVisibility;

    import dmd.attrib;
    import core.stdc.stdio;

    this()
    {
        syms = new ASTCodegen.Dsymbols();
    }

    override void visit(ASTCodegen.Dsymbol ds)
    {
        //printf("ds: %s (%s)\n", ds.toChars(), astTypeName(ds).ptr);
        ds.accept(this);
    }

    override void visit(ASTCodegen.AttribDeclaration ad)
    {
        //printf("ad: %s  (%s)\n", ad.toChars(), astTypeName(ad).ptr);
        ad.accept(this);
    }

    override void visit(ASTCodegen.LinkDeclaration ld)
    {
        //printf("ld: %s, (%s)\n", ld.toChars(), astTypeName(ld).ptr);
        auto oldLinkage = currentLinkage;
        currentLinkage = ld.linkage;
        scope(exit) currentLinkage = oldLinkage;

        foreach(s;*ld.decl)
        {
            //printf("s: %s, (%s)\n", s.toChars(), astTypeName(s).ptr);
            s.accept(this);
        }
    }

    override void visit(ASTCodegen.VisibilityDeclaration vd)
    {
        //printf("vd: %s, (%s)\n", vd.toChars(), astTypeName(vd).ptr);
        auto oldVisibilty = currentVisibility;
        currentVisibility = vd.visibility;
        scope(exit) currentVisibility = oldVisibilty;

        foreach(s;*vd.decl)
        {
            //printf("s: %s, (%s)\n", s.toChars(), astTypeName(s).ptr);
            s.accept(this);
        }
    }

    override void visit(ASTCodegen.StorageClassDeclaration sd)
    {
        //printf("sd: %s, (%s)\n", sd.toChars(), astTypeName(sd).ptr);
        auto oldStorageClass = currentStorageClass;
        currentStorageClass  = sd.stc;
        scope(exit) currentStorageClass = oldStorageClass;

        foreach(s;*sd.decl)
        {
            //printf("s: %s, (%s)\n", s.toChars(), astTypeName(s).ptr);
            s.accept(this);
        }
    }

    override void visit(ASTCodegen.FuncDeclaration fd)
    {
        fd.linkage = currentLinkage;
        fd.storage_class = currentStorageClass;
        fd.visibility = currentVisibility;
        syms.push(fd);
    }

    override void visit(ASTCodegen.VarDeclaration vd)
    {
        vd.linkage = currentLinkage;
        vd.storage_class = currentStorageClass;
        vd.visibility = currentVisibility;
        syms.push(vd);
    }

    override void visit(ASTCodegen.Import imp)
    {
        if (currentVisibility.kind == Visibility.kind.public_)
        {
            imp.load(null);
            auto mod = imp.mod;
            mod.importAll(null);
            mod.dsymbolSemantic(null);
            foreach(m;*mod.members)
            {
                //printf("m: %s, (%s)\n", m.toChars(), astTypeName(m).ptr);
                if (!m.isImport()) m.accept(this);
            }
        }
        // else imp.accept(this);
    }

    override void visit(ASTCodegen.ClassDeclaration cd)
    {
        cd.dsymbolSemantic(null);
        cd.type.trySemantic(Loc.initial, null);
        cd.storage_class = currentStorageClass;
        cd.visibility = currentVisibility;

        syms.push(cd);
    }

    override void visit(ASTCodegen.EnumDeclaration ed)
    {
        ed.dsymbolSemantic(null);
        syms.push(ed);
    }
}
