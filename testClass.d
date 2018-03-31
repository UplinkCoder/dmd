class C 
{
  int i() {return 1;}
}

class D : C
{
  override int i() {return 2;}
  float f() { return 1.0f; }
}

class E : D
{
  override int i() {return 3;}
  override float f() { return 2.0f; }
}


int testClassStuff ()
{
  C c1, c2, c3;
  D c4;
  c1 = new C();
  c2 = new D();
  c3 = new E();

  D e = new E();
  assert(cast(int)e.f() == 2);

  return c1.i + c2.i + c3.i;
}
static assert(testClassStuff == 1 + 2 + 3);
