class C 
{
  // this is ma base yo!
  int base;

  int i() {return 1;}
}

class D : C
{
 // this is ma' derived field yo!
 int d;
 override int i() {return 2;}
 float f() { return 1.0f; }
}

class E : D
{
 this() { dbl = 2.0f; }
 // this is ma' derived field yo!
 double dbl;
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
  pragma(msg, () {E e = new E(); return e.f(); } ());

  return c1.i + c2.i + c3.i;
}


static assert(testClassStuff == 1 + 2 + 3);
