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
}

class E : C
{
 // this is ma' derived field yo!
 double dbl;
 override int i() {return 3;}
}


int testClassStuff ()
{
  C c1, c2, c3;
  c1 = new C();
  c2 = new D();
  c3 = new E();

  return c1.i + c2.i + c3.i;
}


static assert(testClassStuff == 1 + 2 + 3);
