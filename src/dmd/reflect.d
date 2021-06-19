module dmd.reflect;
import dmd.func;

enum REFLECT
{
    Invalid,

    declarationsFromTokenString,

}
import dmd.errors;
import dmd.tokens;
import dmd.id;
import dmd.expression;
import dmd.arraytypes;
import dmd.dinterpret;
import dmd.globals;
import dmd.astcodegen;
import core.stdc.stdio;
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
    const id1 = md.packages[0];
    if (id1 != Id.core)
        return REFLECT.Invalid;

    const id2 = (md.packages.length >= 2) ? md.packages[1] : md.id;
    //const id3 = (md.packages.length >= 3) ? md.packages[2] : md.id; // ignored for now
    const id4 = fd.ident;

    if (id2 == Id.reflect)
    {
        if (id4 == Id.declarationsFromTokenString) return REFLECT.declarationsFromTokenString;
    }

    return REFLECT.Invalid;
}

Expression eval_reflect(const ref Loc loc, REFLECT reflect_kind, Expressions* args)
{
    final switch(reflect_kind)
    {
        case REFLECT.Invalid :
            return null;
        case REFLECT.declarationsFromTokenString :
        {
            initialize();
            import dmd.parse;

            assert((*args).length == 1);
            Expression args0 = (*args)[0];
            import core.stdc.stdio;
            auto se = args0.isStringExp();
            assert(se);
            auto code = se.peekString();
            scope p = new Parser!ASTCodegen(loc, null, code, false);
            printf("string: %s\n", code.ptr);
            p.nextToken();
            auto mod = p.parseModule();
            import dmd.dmodule;
            if (mod.length == 1)
                mod = expandDecls((*mod)[0]);

            printf("declDefs: %s\n", mod.toChars());
            // parse()
            return null;
        }
    }
}

import dmd.visitor;
import dmd.ctfeexpr;
import dmd.dmodule;
import dmd.dimport;
import dmd.identifier;
import dmd.dsymbolsem;

void initialize()
{
    __gshared Import impCoreReflect = null;
    __gshared Identifier[2] coreReflectID;
    if (!impCoreReflect)
    {
        printf("Intializing tables for core.reflect\n");
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
    }

}

__gshared ASTCodegen.Dsymbols* core_reflect = new ASTCodegen.Dsymbols;
__gshared ASTCodegen.ClassDeclaration[] core_reflect_classes;
void fillReflectClasses()
{
    foreach(m;*core_reflect)
    {
        if (auto cd = m.isClassDeclaration())
        {
            cd.dsymbolSemantic(null);
            core_reflect_classes ~= cd;
            import core.stdc.stdio;
            printf("cd: %s\n", cd.toChars());
        }
    }
}

ASTCodegen.Dsymbols* expandDecls(ASTCodegen.Dsymbol s)
{
    scope expandVisitor = new ExpandDeclVisitor();
    s.accept(expandVisitor);
    return expandVisitor.syms;
}
import dmd.dsymbol;
import dmd.dstruct;
import dmd.dclass;

ClassReferenceExp makeReflectionClassLiteral(FuncDeclaration fd)
{
    initialize();

    ClassReferenceExp result;
    StructLiteralExp data;
    ClassDeclaration cd;
    foreach(c;core_reflect_classes)
    {
        if (c.toString == "FunctionDeclaration")
        {
            cd = c;
            break;
        }
    }
    assert(cd);

    Expressions* elements = new Expressions(3);
    // first element is core.reflect.type.FunctionType
    assert((*cd.members)[0].toString() == "type", (*cd.members)[0].toString());

    // second element is core.reflect.decl.VariableDeclaration[] parameters;
    assert((*cd.members)[1].toString() == "parameters", (*cd.members)[1].toString());
    // third element is core.reflect.stmt.Statement fbody
    assert((*cd.members)[2].toString() == "fbody", (*cd.members)[2].toString());
/+
    data = new StructLiteralExp(Loc.initial, cast(StructDeclaration)cd, elements);
    result = new ClassReferenceExp(Loc.initial, data, cd.type);
+/
    return result;
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
    import dmd.asttypename;

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
        /*refl_syms.push(*/if (core_reflect_classes.length) makeReflectionClassLiteral(fd)/*)*/;
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
