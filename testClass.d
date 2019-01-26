class B {}

class C : B
{
  int _i = 2;
  int i() {return ( 1); }
}

class D : C
{
  override int i() {return 2;}
  float f() { return 1.0f; }
}

class E : D
{
//  int x,y,z,w;
  int _i;
  this(int _i)
  {
    this._i = 3;
  }
  override int i() {return _i;}
  override float f() { return 2.0f; }
}


int testClassStuff ()
{
  B b1;
  C c1, c2, c3;
  D c4;
  c1 = new C();
  c2 = new D();
  c3 = new E(3);
  b1 = new D();

  D e = new E(3);
  assert(cast(int)e.f() == 2);
  assert(c2 is c2, "Identity is broken ?");
  assert((cast(D)c3), "Dynamic cast not working");
  assert(!(cast(E)c2), "Dynamic cast not working");
  assert((cast(C)b1), "Dynamic cast not working");

  return c1.i + c2.i + c3.i;
}
static assert(testClassStuff == 1 + 2 + 3);
static assert(testClassStuff == 1 + 2 + 3);
static assert(testClassStuff == 1 + 2 + 3);
