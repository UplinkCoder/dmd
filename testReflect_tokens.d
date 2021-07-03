import dmd.tokens;
version(UseCoreReflectRT)
{
    version = UseCoreReflect;
}
version(UseCoreReflectCT)
{
    version = UseCoreReflect;
}

version (UseCoreReflect)
{
  import core.reflect.reflect;
  static immutable e = cast(immutable EnumDeclaration) nodeFromName("TOK");

  string rep (const EnumDeclaration e)
  {
    import std.conv;
    string result;
    result ~= "enum " ~ e.name ~ " {\n";

    foreach(m;e.members)
    {
        IntegerLiteral l = cast(IntegerLiteral) m.value;
        result ~= "  " ~ m.name ~ " = " ~ to!string(l.value) ~ ",\n";
    }

    result ~= "}";
    return result;
  }
}

version (UseCoreReflectRT)
{
  void main()
  {
    import core.stdc.stdio;
    printf("%s\n", (rep(e) ~ "\0").ptr);
  }
}
version (UseCoreReflectCT)
{
  enum x = rep(e);
}

version(UseTraitsRT)
{
  alias EnumT = TOK;
  enum membersEnum = __traits(allMembers, EnumT);
  string[] memberNames = [membersEnum];
  long[] memberValues =
  (() {
    typeof(memberValues) result;
    result.length = membersEnum.length;
    foreach(i, m;membersEnum)
    {
      result[i] = __traits(getMember, EnumT, m);
    }
    return result;
  } ());
  void main()
  {
    import core.stdc.stdio;
    import std.conv;
    string result;
    result ~= "enum " ~ EnumT.stringof ~ "{\n";

    foreach(i; 0 .. memberValues.length)
    {
      result ~= "  " ~ memberNames[i] ~ " = " ~ to!string(memberValues[i]) ~ ",\n"; 
    }

    result ~= "}\n";
    printf("%s\n", (result ~ "\0").ptr);
  }
}

version(UseTraitsCT)
{
  enum members = __traits(allMembers, TOK);

  enum x = (
    () {
      import std.conv;
      string result;

      result ~= "enum " ~ TOK.stringof ~ "{\n";

      static foreach(m;members)
      {
          result ~= "  " ~ m ~ " = " ~ to!string(cast(int)mixin("TOK." ~ m)) ~ ",\n";
      }

      result ~= "}";
      return result;
    } ()
  );
}
