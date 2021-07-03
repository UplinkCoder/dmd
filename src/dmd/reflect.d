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
    printf("arguments: %s\n", args.toChars());
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
                Scope* tmp;
                dsymbolSemantic((*mod)[0], tmp = lookupScope.push());
                mod = expandDecls((*mod)[0]);
                tmp.pop();
                //lookupScope = (*mod)[0].scope_;

            }
            auto expressions = new Expressions(mod.length);
            foreach(i, d;*mod)
            {
                if (auto fd = d.isFuncDeclaration())
                {
                    (*expressions)[i] = makeReflectionClassLiteral(fd, lookupScope, true);
                }
                else if (auto vd = d.isVarDeclaration())
                {
                    (*expressions)[i] = makeReflectionClassLiteral(vd, lookupScope, true);
                }
                else if (auto ed = d.isEnumDeclaration())
                {
                    (*expressions)[i] = makeReflectionClassLiteral(ed, lookupScope, true);
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
            auto scope_value_index = 0 + ReflectionVisitor.getCd("Node").fields.length;
            auto scope_value = (*lookupScopeExp.isClassReferenceExp().value.elements)[scope_value_index];
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
                result = makeReflectionClassLiteral(sym, mylookupScope, false);
            }
            else
            {
                result = new NullExp(loc, ReflectionVisitor.getCd("Node").type);
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
            scope expandVisitorX = new ExpandDeclVisitor(true);
            if (sym != s) sym.accept(expandVisitorX);
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
import dmd.statement;

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
    Expressions* elements = new Expressions();
    if (cd.baseClass.fields.length)
    {
        elements.push(IntegerExp.literal!0);
        (*elements)[0].type = Type.tuns64;
    }
    assert((cd.fields)[0].toString() == "internalPointer", (cd.fields)[0].toString());
    elements.push = new IntegerExp(loc, ivalue, Type.tvoidptr);
    auto data = new StructLiteralExp(loc, cast(StructDeclaration)cd, elements);
    auto result = new ClassReferenceExp(loc, data, cd.type.immutableOf());

    return result;
}

ClassReferenceExp makeReflectionClassLiteral(ASTNode n, Scope* sc, bool ignoreImports)
{
    import dmd.asttypename;

    bool dont_print = false;
    auto typename = n.astTypeName();
    if (typename == "TypeClass")
    {
        auto tc = cast(TypeClass) n;
        auto sym = cast(ClassDeclaration) tc.sym;
        auto d = cast(Declaration) sym;
        auto t = cast(Type) tc;
        if (!t.deco) //asm { int 3; }
            dont_print = true;
    }
    if (!dont_print)
    {
    printf("+%s: '%s' {astTypeName: %s}}\n",
        __FUNCTION__.ptr, n.toChars(), n.astTypeName().ptr);
    scope(exit)
        printf("-%s: '%s' {astTypeName: %s}}\n",
            __FUNCTION__.ptr, n.toChars(), n.astTypeName().ptr);
    }
    if (auto cre = (cast(void*) n) in ReflectionVisitor.cache)
    {
        printf("Found '%s' in cache\n", n.toChars());
        return *cast(ClassReferenceExp*)cre;
    }

    scope reflectionVisitor = new ReflectionVisitor(n, sc, ignoreImports);
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
    bool ignoreImports = false;

    this(ASTNode n, Scope* sc, bool ignoreImports, Loc loc = Loc.initial)
    {
        elements = new Expressions();
        this.ignoreImports = ignoreImports;
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

    ClassReferenceExp placeholder(ClassDeclaration pcd = null, int line = __LINE__)
    {
        auto ncd = (pcd ? pcd : cd);
        printf("[+%d] Making placeholder of class: %s\n", line, ncd.toChars());

        auto placeholder_data = new StructLiteralExp(loc, cast(StructDeclaration)ncd, elements);
        return new ClassReferenceExp(loc, placeholder_data, ncd.type.immutableOf());
    }

    void finalize()
    {
        printf("before finalisation: \n");

        enum debug_core_reflect = 1;
        static if (debug_core_reflect)
        {
            uint currentField = 0;
            int currentElement = cast(int) (elements.length - cd.fields.length);
            assert(currentElement >= 0, "cd.fields longer than elements cd:" ~ cd.toString());
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
                assert(currentElement >= 0, "at base: '" ~ test.toString() ~ "' fields longer than elements cd:" ~ cd.toString());
            }
        }
        data = new StructLiteralExp(loc, cast(StructDeclaration)cd, elements);
        if (auto placeholder = (cast(void*)node) in cache)
        {
            placeholder.value = data;
            placeholder.type = cd.type.immutableOf();
            result = *placeholder;
        }
        else
        {
            result = new ClassReferenceExp(loc, data, cd.type.immutableOf());
        }

        printf("finalized result: %s\n", result.toChars());
    }

    override void visit(ASTCodegen.TemplateDeclaration d)
    {
    }

    override void visit(ASTCodegen.TypeEnum t)
    {
        assert(leaf);
        cd = getCd("TypeEnum");

        leaf = 0;
        handleType(t);
        leaf = 1;

        auto enumDecl = makeReflectionClassLiteral(t.sym, lookupScope, ignoreImports);
        fillField((cd.fields)[0], "sym", elements, enumDecl);

        if (leaf)
            finalize();
    }

    override void visit(StringExp e)
    {
        assert(leaf);
        auto oldLeaf = leaf;
        leaf = 0;
        handleExp(e);
        leaf = oldLeaf;
        cd = getCd("StringLiteral");
        e = cast(StringExp)e.copy();
        e.type = Type.tstring;
        fillField((cd.fields)[0], "string_", elements, e);
        if (leaf)
            finalize();
    }

    override void visit(IntegerExp e)
    {
        if (!e.type.isintegral())
            return ;

        assert(leaf);
        auto oldLeaf = leaf;
        leaf = 0;
        handleExp(e);
        leaf = oldLeaf;
        cd = getCd("IntegerLiteral");
        auto oldType = e.type;
        e.type = Type.tuns64;
        fillField((cd.fields)[0], "value", elements, e);
        e.type = oldType;
        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.TypeClass t)
    {
        assert(leaf);

        cd = getCd("TypeClass");

        leaf = 0;
        handleType(t);
        leaf = 1;

        import dmd.asttypename;
        auto sym = makeReflectionClassLiteral(t.sym, lookupScope, ignoreImports);
        fillField(cd.fields[0], "sym", elements, sym);

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.TypeStruct t)
    {
        assert(leaf);

        cd = getCd("TypeStruct");
        cache[cast(void*)t] = placeholder();
        auto struct_sym = t.sym;

        leaf = 0;
        handleType(t);
        leaf = 1;

        auto sym = makeReflectionClassLiteral(struct_sym, lookupScope, ignoreImports);
        fillField(cd.fields[0], "sym", elements, sym);

        if (leaf)
            finalize();
    }

    override void visit(FuncExp e)
    {
        assert(leaf);

        cd = getCd("FunctionLiteral");

        assert(leaf);
        auto oldLeaf = leaf;
        leaf = 0;
        handleExp(e);
        leaf = oldLeaf;

        cd = getCd("FunctionLiteral");

        const nParameters = e.fd.parameters ? e.fd.parameters.length : 0;

        Expressions* parameterElements = new Expressions(nParameters);
        if (nParameters) foreach(i, p; *e.fd.parameters)
        {
            (*parameterElements)[i] = makeReflectionClassLiteral(p, lookupScope, ignoreImports);
        }
        auto parameterArrray = new ArrayLiteralExp(loc, (cd.fields[0]).type, parameterElements);

        fillField((cd.fields)[0], "parameters", elements, parameterArrray);

        auto oldlookupScope = lookupScope;
        lookupScope = e.fd._scope;
        // third element is core.reflect.stmt.Statement fbody
        // TODO FIXME: serialize the body statement into here
        auto fbody = ((e.fd.fbody && emitFunctionBodies) ? makeReflectionClassLiteral(e.fd.fbody, lookupScope, ignoreImports)
            : new NullExp(loc, getCd("Statement").type));

        fillField((cd.fields)[1], "fbody", elements, fbody);
        lookupScope = oldlookupScope;

        if (leaf)
            finalize();
    }


    override void visit(ASTCodegen.TypePointer t)
    {
        cd = getCd("TypePointer");
        handleTypeNext(cast(TypeNext)t);
    }

    override void visit(ASTCodegen.TypeSArray t)
    {
        cd = getCd("TypeArray");
        leaf = 0;
        handleTypeNext(cast(TypeNext)t);
        leaf = 1;

        auto oldType = t.dim.type;
        t.dim.type = Type.tuns32;
        fillField((cd.fields)[0], "dim", elements, t.dim.copy());
        t.dim.type = oldType;

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.TypeDArray t)
    {
        cd = getCd("TypeSlice");
        handleTypeNext(cast(TypeNext)t);
    }


    void handleTypeNext(ASTCodegen.TypeNext t)
    {
        auto oldCd = cd;
        scope(exit)
            cd = oldCd;

        // assert that we came from direct child of of type next;
        cd = cd.baseClass;
        assert(cd.toString() == "TypeNext", cd.toString());

        auto oldLeaf = leaf;
        import dmd.typesem;
        auto resolved = typeSemantic(t.next, loc, lookupScope);
        if (resolved.ty != TY.Terror)
        {
            t.next = resolved;
        }

        leaf = 0;
        handleType(t);
        leaf = oldLeaf;

        auto tnext = makeReflectionClassLiteral(resolved, lookupScope, ignoreImports);
        fillField((cd.fields)[0], "nextOf", elements, tnext);

        if (leaf)
            finalize();
    }

    override void visit(TypeIdentifier ti)
    {
        import dmd.typesem;
        // type identifiers are unresolved. try to resolve them!
        // auto t = typeSemantic(ti, loc, lookupScope);
        // we can not run type smenatic here because we don't know if it is a type.
        Dsymbol out_scope;
        auto sym_id = ti.ident;
        auto s = lookupScope.search(loc, sym_id, &out_scope);
        if (s) printf("s: %s\n", s.toChars());
       //  visit(t);
    }

    override void visit(TypeTypeof tt)
    {
        import dmd.typesem;
        // type typeofs are unresolved. try to resolve them!
        auto t = typeSemantic(tt, loc, lookupScope);
        visit(t);
    }


    override void visit(TypeBasic t)
    {
        assert(leaf);
        cd = getCd("TypeBasic");

        leaf = 0;
        handleType(t);
        leaf = 1;

        if (leaf)
            finalize();
    }

    void handleNode(ASTNode n)
    {
        assert(!leaf);
        auto oldCd = cd;
        scope(exit)
            cd = oldCd;

        cd = cd.baseClass;
        assert(cd.toString == "Node");

        static if (is(typeof(n.serial)))
        {
            if (cd.fields.length && cd.fields[0].toString == "serial")
            {
                auto serial = new IntegerExp(loc, n.serial, Type.tuns64);
                fillField((cd.fields)[0], "serial", elements, serial);
            }
        }
    }

    void handleStatement(Statement s)
    {
        assert(!leaf);
        auto oldCd = cd;
        scope(exit)
            cd = oldCd;
        cd = cd.baseClass;
        assert(cd.toString() == "Statement");

        handleNode(s);
    }

    void handleType(Type t)
    {
        assert(!leaf);
        auto oldCd = cd;
        cd = cd.baseClass;
        assert(cd.toString() == "Type");

        t = t.merge2();
        if (cast(void*)t in cache) {}
        else
        {
            auto p = placeholder(oldCd ? oldCd : cd);
            cache[cast(void*)t] = p;
        }

        handleNode(t);

        ulong size;
        uint alignSize;

        if (auto ts = t.isTypeStruct())
        {
            auto sym = ts.sym;
            auto forward_ref =
                (sym.members && !sym.determineFields() && sym.type != Type.terror);

            if (forward_ref)
            {
                size = ~0;
                alignSize = ~0;
            }
            goto LnormalPath;
        }
        else if (auto tf = t.isTypeFunction())
        {
            size = 0;
            alignSize = 0;
        }
        // the following else if is temporary TODO FIXME
        else if (auto tf = t.isTypeEnum())
        {
            size = 0;
            alignSize = 0;
        }
        else
        {
LnormalPath:
            size = t.size();
            alignSize = t.alignsize();
        }

        auto kind = makeString(t.kind());//tyToTypeKind(cast(ENUMTY)t.ty);
        fillField((cd.fields)[0], "kind", elements, kind);

        auto alignSizeExp = new IntegerExp(loc, alignSize, Type.tuns32);
        fillField((cd.fields)[1], "alignSize", elements, alignSizeExp);

        auto sizeExp = new IntegerExp(loc, size, Type.tuns64);
        fillField((cd.fields)[2], "size", elements, sizeExp);

        {
            if (!t.deco)
            {

            }
            auto identifier = makeString((t.deco ? t.toChars() : null), loc);

            fillField((cd.fields)[3], "identifier", elements, identifier);
        }

        if (leaf)
            finalize();
        else
            cd = oldCd;
    }

    private extern (D) void fillField(VarDeclaration vd, string fieldName, Expressions* elements, Expression exp,
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

        elements.push = exp;
    }

    override void visit(ASTCodegen.Parameter p)
    {
        assert(leaf);

        cd = getCd("FunctionParameter");

        leaf = 0;
        handleNode(p);
        leaf = 1;

        auto type = makeReflectionClassLiteral(p.type, lookupScope, ignoreImports);
        fillField((cd.fields)[0], "type", elements, type);

        Expression identifier;
        if (p.ident)
        {
            identifier = makeString(p.ident.toChars(), loc);
        }
        else
        {
            identifier = new NullExp(loc, Type.tstring);
        }

        fillField((cd.fields)[1], "identifier", elements, identifier);

        finalize();
    }

    void handleDeclaration(Declaration d)
    {

        auto oldCd = cd;
        cd = getCd("Declaration");

        handleNode(d);

        fillField((cd.fields)[0], "name", elements, makeString(d.ident ? d.ident.toChars() : null, loc));

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

    void handlePackage(ASTCodegen.Package p)
    {
        auto oldcd = cd;
        scope(exit)
            cd = oldcd;

        cd = getCd("Package");

        printf("[handlePackage] oldcd: %s\n", (oldcd ? oldcd.toChars() : "null") );

        auto oldleaf = leaf;
        leaf = 0;
        handleScopeSymbol(p);
        leaf = oldleaf;

        // here we would handle any special fields for Package

        if (leaf)
            finalize();
    }

    void handleScopeSymbol(ASTCodegen.ScopeDsymbol s)
    {
        assert(!leaf);

        auto oldcd = cd;
        scope(exit)
            cd = oldcd;

        printf("[handleScopeSymbol] oldcd: %s\n", oldcd.toChars());

        cd = getCd("ScopeSymbol");

        handleSymbol(s);

        // hanlde Symbol has already put a placeholder
        // cache[cast(void*)s] = placeholder(oldcd ? oldcd : cd);
        {
            auto memberCdType = getCd("Symbol").type;

            auto oldIgnoreImports = ignoreImports;
            scope(exit)
                ignoreImports =oldIgnoreImports;
            ignoreImports = true;

            auto nMembers = (s.members ? s.members.length : 0);
            Expressions* membersElements = new Expressions();

            if (nMembers) foreach(i, m;*s.members)
            {
                import dmd.asttypename;
                auto r  = makeReflectionClassLiteral(m, s._scope, ignoreImports);
                if (!r)
                    printf("Reflection class createion not handled for: '%s' (%s)\n", m.toChars(), astTypeName(m).ptr);
                else
                    membersElements.push = r;
            }

            auto members = new ArrayLiteralExp(loc, memberCdType.arrayOf(), membersElements);
            fillField(cd.fields[0], "members", elements, members);
        }
    }

    void handleSymbol(ASTCodegen.Dsymbol s)
    {
        assert(!leaf);
        auto oldcd = cd;
        scope(exit)
            cd = oldcd;

        cd = getCd("Symbol");
        if (cast(void*)s in cache) {}
        else
        {
            import dmd.asttypename;
            printf("about to make placehoder for: '%s' (%s)\n", s.toChars(), astTypeName(s).ptr);
            cache[cast(void*)s] = placeholder(oldcd ? oldcd : cd);
        }
        assert(!leaf);

        handleNode(s);

        {
            Expression _scope;
            if (s._scope)
            {
                _scope = makeReflectionClassLiteral(s._scope, loc);
            }
            else
            {
                _scope = new NullExp(loc, getCd("Scope").type);
            }

            fillField(cd.fields[0], "_scope", elements, _scope);
        }

        {
            auto identifier = makeString(s.ident ? s.ident.toChars() : null);
            fillField(cd.fields[1], "identifier", elements, identifier);
        }

        {
            Expression parent;
            if (s.parent)
            {
                if (s.isImport())
                    goto LnullParent;
                parent = makeReflectionClassLiteral(s.parent, lookupScope, ignoreImports);
            }
            else
            {
            LnullParent:
                parent = new NullExp(loc, getCd("Symbol").type);
            }
            assert(parent);

            fillField(cd.fields[2], "parent", elements, parent);
        }
    }

    override void visit(ASTCodegen.Package p)
    {
        handlePackage(p);
    }

    override void visit(ASTCodegen.Module m)
    {
        auto oldIgnoreImports = ignoreImports;
        scope (exit)
            ignoreImports = oldIgnoreImports;
        ignoreImports = true;

        assert(leaf);
        cd = getCd("Module");
        cache[cast(void*)m] = placeholder(cd);

        {
            leaf = 0;
            handlePackage(m);
            leaf = 1;
        }

        {
            auto source_filename = makeString(m.srcfile.toChars());
            fillField(cd.fields[0], "source_filename", elements, source_filename);
        }
        {
            auto nStringImportFilenames = m.contentImportedFiles.length;
            auto string_import_filenameElements = new Expressions(nStringImportFilenames);
            foreach (i, n;m.contentImportedFiles)
            {
                (*string_import_filenameElements)[i] = makeString(n);
            }
            auto string_import_filenames = new ArrayLiteralExp(loc, Type.tstring.arrayOf, string_import_filenameElements);
            fillField(cd.fields[1], "string_import_filenames", elements, string_import_filenames);
        }

        {
            auto DeclDefsCdType = getCd("Symbol").type;

            auto nDeclDefs = (m.decldefs ? m.decldefs.length : 0);
            Expressions* declDefElements = new Expressions(nDeclDefs);

            if (nDeclDefs) foreach(i, d;*m.decldefs)
            {
                (*declDefElements)[i] = makeReflectionClassLiteral(d, d._scope, ignoreImports);
            }

            auto declDefs = new ArrayLiteralExp(loc, DeclDefsCdType.arrayOf(), declDefElements);
            fillField(cd.fields[2], "declDefs", elements, declDefs);
        }

        if (leaf)
            finalize();
    }


    override void visit(ASTCodegen.Import imp)
    {
        if (ignoreImports)
        {
            printf("Ignoring import\n");
            return ;
        }
        assert(leaf);
        cd = getCd("Import");
        cache[cast(void*)imp] = placeholder();

        {
            leaf = 0;
            handleSymbol(imp);
            leaf = 1;
        }

        {
            auto packageElements = new Expressions(imp.packages.length);
            foreach(i, p;imp.packages)
            {
                (*packageElements)[i] = makeString(p.toChars());
            }
            auto packages = new ArrayLiteralExp(loc, Type.tstring.arrayOf(), packageElements);
            fillField((cd.fields)[0], "packages", elements, packages);
        }

        {
            auto moduleIdentifier = makeString(imp.id.toChars());
            fillField((cd.fields)[1], "moduleIdentifier", elements, moduleIdentifier);
        }

        {
            auto isStatic = IntegerExp.createBool(imp.isstatic != 0);
            fillField((cd.fields)[2], "isStatic", elements, isStatic);
        }

        {
            auto visibility = VisibilityKindtoVisibility(imp.visibility.kind);
            fillField((cd.fields)[3], "visibility", elements, visibility);
        }

        {
            Expression mod;
            if (imp.mod)
            {
                printf("Getting mod via 'makeReflectionClassLiteral'\n");
                mod = makeReflectionClassLiteral(imp.mod, lookupScope, ignoreImports);
            }
            else
            {
                mod = new NullExp(loc, getCd("Module").type);
            }

            // printf("mod: %s ... mod.type: %s\n", mod.toChars(), mod.type.toChars());
            fillField((cd.fields)[4], "mod", elements, mod);
        }

        if (leaf)
            finalize();
    }

    override void visit (ASTCodegen.TypeFunction ft)
    {
        assert(leaf);

        cd = getCd("FunctionType");
        auto base = cd.baseClass;

        leaf = 0;
        handleType(ft);
        leaf = 1;

        auto returnType = makeReflectionClassLiteral(ft.next, lookupScope, ignoreImports);
        fillField((cd.fields)[0], "returnType", elements, returnType);

        auto nParams = ft.parameterList.length;

        Expressions* parameterTypeElements = new Expressions(nParams);
        auto cParameter = getCd("FunctionParameter");
        foreach(i, p;ft.parameterList)
        {
            auto para = makeReflectionClassLiteral(p, lookupScope, ignoreImports);
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

    static Expression opToBinaryOp(TOK op)
    {
        auto ed = getEd("BinaryOp");
        string lookFor;

        switch(op)
        {
            case TOK.add :
                lookFor = "Add";
                break;
            case TOK.min :
                lookFor = "Sub";
                break;
            case TOK.mul :
                lookFor = "Mul";
                break;
            case TOK.div :
                lookFor = "Div";
                break;
            case TOK.mod :
                lookFor = "Mod";
                break;
            case TOK.leftShift :
                lookFor = "Shl";
                break;
            case TOK.rightShift :
                lookFor = "Shr";
                break;
            case TOK.concatenate :
                lookFor = "Cat";
                break;
            case TOK.in_ :
                lookFor = "In";
                break;
            case TOK.equal :
                lookFor = "Eq";
                break;
            case TOK.notEqual :
                lookFor = "Neq";
                break;
            case TOK.identity :
                lookFor = "Is";
                break;
            case TOK.notIdentity :
                lookFor = "Nis";
                break;
            default : assert(0, "Translation from TOK." ~ enumToString(op) ~ " to BinaryOp is not implemented.");
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

        assert(0, "BinaryOp." ~ lookFor ~ " could not be found in BinaryOp enum");
    }

    static Expression VisibilityKindtoVisibility(Visibility.Kind visibility)
    {
        EnumDeclaration ed = getEd("Visibility");
        string lookFor;

        final switch(visibility)
        {
            case visibility.undefined :
                lookFor = "Undefined";
                break;
            case visibility.none :
                lookFor = "NoAccess";
                break;
            case visibility.private_ :
                lookFor = "Private";
            break;
            case visibility.package_ :
                lookFor = "Package";
            break;
            case visibility.protected_ :
                lookFor = "Protected";
            break;
            case visibility.public_ :
                lookFor = "Public";
            break;
            case visibility.export_ :
                lookFor = "Export";
           break;
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

        assert(0, "Visibility." ~ lookFor ~ " could not be found in Visibility enum");

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

        assert(0, "Linkage." ~ lookFor ~ " could not be found in Linkage enum");

    }

    override void visit(ASTCodegen.Initializer i)
    {

    }

    override void visit(ASTCodegen.ExpInitializer i)
    {
        // just a passthrough
        import dmd.asttypename;
        printf("astTypeName(i.exp): %s\n", astTypeName(i.exp).ptr);
        i.exp.accept(this);
    }

    override void visit(ASTCodegen.EnumDeclaration ed)
    {
        cd = getCd("EnumDeclaration");

        auto oldLeaf = leaf;
        leaf = 0;
        handleDeclaration(cast(Declaration)ed);
        leaf = oldLeaf;

        auto type = makeReflectionClassLiteral(ed.type, lookupScope, ignoreImports);
        fillField(cd.fields[0], "type", elements, type);

        auto memberCd = getCd("EnumMember");

        auto nMembers = (ed.members ? ed.members.length : 0);
        Expressions* enumMembers = new Expressions(nMembers);

        if (nMembers) foreach(i, m; *ed.members)
        {
            auto em = m.isEnumMember();
            (*enumMembers)[i] = makeReflectionClassLiteral(em, lookupScope, ignoreImports);
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
        handleDeclaration(cast(Declaration)m);
        leaf = oldLeaf;

        auto value = makeReflectionClassLiteral(m.value(), lookupScope, ignoreImports);
        fillField((cd.fields)[0], "value", elements, value);

        if (leaf)
            finalize();
    }


    override void visit(ASTCodegen.IdentifierExp e)
    {
        import dmd.expressionsem;
        e = cast(IdentifierExp)expressionSemantic(e, lookupScope);
        assert(!e.isIdentifierExp(), "Unresolved expression detected");
    }


    void handleExp(Expression e)
    {
        assert(!leaf);
        auto oldCd = cd;
        cd = getCd("Expression");

        handleNode(e);

        if(!e.type)
        {
            import dmd.expressionsem;
            e = e.expressionSemantic(lookupScope);
            assert(e.type, "Expression '" ~ e.toString ~ "' is supposed to have a type");
        }
        auto type = makeReflectionClassLiteral(e.type, lookupScope, ignoreImports);
        assert(type, e.toString());
        fillField((cd.fields)[0], "type", elements, type);

        if (!leaf)
            cd = oldCd;
    }

    override void visit (ASTCodegen.VarDeclaration vd)
    {
        cd = getCd("VariableDeclaration");
        cache[cast(void*)vd] = placeholder();

        import dmd.dsymbolsem;
        dsymbolSemantic(vd, lookupScope);
        auto oldLeaf = leaf;
        leaf = 0;
        handleDeclaration(cast(Declaration)vd);
        leaf = oldLeaf;
        {
            assert(vd.type);
            auto type = makeReflectionClassLiteral(vd.type, lookupScope, ignoreImports);
            fillField((cd.fields)[0], "type", elements, type);
        }
        {
            Expression _init;
            if (vd._init)
            {
                _init = makeReflectionClassLiteral(vd._init, lookupScope, ignoreImports);
                if (!_init)
                {
                    printf("Couldn't handle init: '%s'\n", vd._init.toChars());
                    goto LnullInit;
                }
            }
            else
            {
            LnullInit:
                _init = new NullExp(loc, getCd("Expression").type);
            }
            fillField((cd.fields)[1], "_init", elements, _init);
        }
        {
            Expression offset;
            if (!vd.offset)
            {
                // special casing offset 0 because it's often the case
                offset = IntegerExp.literal!0;
            }
            else
            {
                 offset = IntegerExp.create(loc, vd.offset, Type.tuns64);
            }
            fillField((cd.fields)[2], "offset", elements, offset);
        }
        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.CompoundStatement ce)
    {
        cd = getCd("BlockStatement");

        handleStatement(ce);

        auto nStatements = ce.statements ? ce.statements.length : 0;
        Expressions* statementElems = new Expressions(0);
        if (nStatements) foreach(i, s; *ce.statements)
        {
            auto refl_stmt = makeReflectionClassLiteral(s, lookupScope, ignoreImports);
            if (!refl_stmt)
            {
                import dmd.asttypename;
                printf("Statement type not handled by reflection: '%s' (%s)\n", s.toChars(), s.astTypeName().ptr);
            }
            else
                statementElems.push = refl_stmt;
        }
        auto statements =
            new ArrayLiteralExp(loc, getCd("Statement").type.arrayOf, statementElems);

        fillField(cd.fields[0], "statements", elements, statements);

        if (leaf)
            finalize();
    }

    override void visit(ImportStatement s)
    {
        assert(leaf);

        cd = getCd("ImportStatement");

        auto nImports = s.imports ? s.imports.length : 0;

        Expressions* importElements = new Expressions();
        if (nImports) foreach(i;*s.imports)
        {
            import dmd.dimport;
            auto imp = i.isImport();
            assert(imp);
            auto imp_refl = makeReflectionClassLiteral(imp, lookupScope, ignoreImports);
            if (imp_refl)
            {
                importElements.push = imp_refl;
            }
        }

        auto imports = new ArrayLiteralExp(loc, getCd("Symbol").type.arrayOf(), importElements);
        fillField(cd.fields[0], "imports", elements, imports);

        if (leaf)
            finalize();
    }


    override void visit(ASTCodegen.ReturnStatement s)
    {
        assert(leaf);

        cd = getCd("ReturnStatement");
        auto exp = s.exp ? makeReflectionClassLiteral(s.exp, lookupScope, ignoreImports) : new NullExp(loc, getCd("Expression").type);
        fprintf(stderr, "ret.loc: %s\n", s.loc.toChars());
        fprintf(stderr, "exp.typename %s\n", astTypeName(s.exp).ptr);

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
        handleExp(e);
        leaf = oldLeaf;

        auto var = makeReflectionClassLiteral(e.var, lookupScope, ignoreImports);
        fillField((cd.fields)[0], "var", elements, var);

        if (leaf)
            finalize();
    }

    import dmd.asttypename;
    override void visit(Expression e)
    {
        assert(0, "Expression not supported " ~ astTypeName(e) ~ ".");
    }

    override void visit(CallExp e)
    {
        assert(leaf);
        cd = getCd("CallExpression");
        leaf = 0;
        handleExp(e);
        leaf = 1;

        if (leaf)
            finalize();
    }

    override void visit(NullExp e)
    {
        assert(leaf);
        cd = getCd("NullExpression");
        leaf = 0;
        handleExp(e);
        leaf = 1;

        finalize();
    }

    void handleBinExp(BinExp e)
    {
        assert(leaf);
        cd = getCd("BinaryExpression");

        auto oldLeaf = leaf;
        leaf = 0;
        handleExp(e);
        leaf = oldLeaf;

        {
            auto op = opToBinaryOp(e.op);
            fillField(cd.fields[0], "op", elements, op);
        }

        {
            auto left = makeReflectionClassLiteral(e.e1, lookupScope, ignoreImports);
            fillField(cd.fields[1], "left", elements, left);
        }

        {
            auto right = makeReflectionClassLiteral(e.e2, lookupScope, ignoreImports);
            fillField(cd.fields[2], "right", elements, right);
        }

        if (leaf)
            finalize();
    }

    override void visit(AddExp e)
    {
        handleBinExp(e);
    }

    override void visit(MinExp e)
    {
        handleBinExp(e);
    }

    override void visit(MulExp e)
    {
        handleBinExp(e);
    }

    override void visit(DivExp e)
    {
        handleBinExp(e);
    }

    override void visit(ASTCodegen.StructDeclaration d)
    {
        assert(leaf);

        cd = getCd("StructDeclaration");
        cache[cast(void*)d] = placeholder();

        leaf = 0;
        visit(cast(AggregateDeclaration) d);
        leaf = 1;

        if (leaf)
            finalize();
    }

    override void visit(ASTCodegen.ClassDeclaration d)
    {
        assert(leaf);

        cd = getCd("ClassDeclaration");
        cache[cast(void*)d] = placeholder();

        leaf = 0;
        visit(cast(AggregateDeclaration) d);
        leaf = 1;

        auto onStack = IntegerExp.createBool(d.stack);
        fillField(cd.fields[0], "onStack", elements, onStack);

        if (leaf)
            finalize();
    }
    import dmd.aggregate;

    override void visit(ASTCodegen.AggregateDeclaration d)
    {
        assert(!leaf);

        auto oldCd = cd;
        scope(exit) cd = oldCd;
        cd = getCd("AggregateDeclaration");

        handleDeclaration(cast(Declaration)d);

        {
            auto type = makeReflectionClassLiteral(d.type, lookupScope, ignoreImports);
            fillField(cd.fields[0], "type", elements, type);
        }

        {
            auto nFields = d.fields.length;
            Expressions* fieldElements = new Expressions(nFields);
            if (nFields) foreach(i, f; d.fields)
            {
                (*fieldElements)[i] = makeReflectionClassLiteral(f, lookupScope, ignoreImports);
            }
            auto fields = new ArrayLiteralExp(loc, (cd.fields[1]).type, fieldElements);
            fillField(cd.fields[1], "fields", elements, fields);
        }
    }

    override void visit(ASTCodegen.FuncDeclaration fd)
    {
        cd = getCd("FunctionDeclaration");
        cache[cast(void*)fd] = placeholder();

        auto oldLeaf = leaf;
        leaf = 0;
        handleDeclaration(cast(Declaration)fd);
        leaf = oldLeaf;

        bool semaDone = fd.functionSemantic();
        bool sema3Done = fd.functionSemantic3();

        auto type = makeReflectionClassLiteral(cast(TypeFunction)fd.type, lookupScope, ignoreImports);
        fillField((cd.fields)[0], "type", elements, type);

        // second element is core.reflect.decl.VariableDeclaration[] parameters;
        assert((cd.fields)[1].toString() == "parameters", (cd.fields)[1].toString());
        const nParameters = fd.parameters ? fd.parameters.length : 0;

        Expressions* parameterElements = new Expressions(nParameters);
        if (nParameters) foreach(i, p; *fd.parameters)
        {
            (*parameterElements)[i] = makeReflectionClassLiteral(p, lookupScope, ignoreImports);
        }
        auto parameterArrray = new ArrayLiteralExp(loc, (cd.fields[1]).type, parameterElements);

        fillField((cd.fields)[1], "parameters", elements, parameterArrray);

        auto oldlookupScope = lookupScope;
        lookupScope = fd._scope;
        // third element is core.reflect.stmt.Statement fbody
        // TODO FIXME: serialize the body statement into here
        auto fbody = ((fd.fbody && emitFunctionBodies) ? makeReflectionClassLiteral(fd.fbody, lookupScope, ignoreImports)
            : new NullExp(loc, getCd("Statement").type));
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

    bool for_core_reflect = false;

    import dmd.attrib;
    import core.stdc.stdio;

    this(bool for_core_reflect = false)
    {
        this.for_core_reflect = for_core_reflect;
        syms = new ASTCodegen.Dsymbols();
    }

    override void visit(ASTCodegen.Dsymbol ds)
    {
        //printf("ds: %s (%s)\n", ds.toChars(), astTypeName(ds).ptr);
        // ds.accept(this);
    }

    override void visit(ASTCodegen.AttribDeclaration ad)
    {
        //printf("ad: %s  (%s)\n", ad.toChars(), astTypeName(ad).ptr);
        // ad.accept(this);
    }

    override void visit(ASTCodegen.LinkDeclaration ld)
    {
         if (!for_core_reflect)
             return ;
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
         if (!for_core_reflect)
             return ;

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
         if (!for_core_reflect)
             return ;

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
        if (for_core_reflect)
        {
            fd.linkage = currentLinkage;
            fd.storage_class = currentStorageClass;
            fd.visibility = currentVisibility;
        }
        syms.push(fd);
    }

    override void visit(ASTCodegen.VarDeclaration vd)
    {
        if (for_core_reflect)
        {
            vd.linkage = currentLinkage;
            vd.storage_class = currentStorageClass;
            vd.visibility = currentVisibility;
        }
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
        // ignore NodeToStringVisitor in core.reflect.reflect
        if (cd.toString == "NodeToStringVisitor")
            return ;

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

private string enumToString(E)(E v)
{
    static assert(is(E == enum),
        "emumToString is only meant for enums");
    final switch (v)
    {
        foreach(m; __traits(allMembers, E))
        {
            mixin("case E." ~ m ~ ": return \"" ~ m ~ "\";");
        }
    }
}
