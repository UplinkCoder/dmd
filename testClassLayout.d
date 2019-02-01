
alias twice = long; alias once = int;
class B {once more = 0x123; twice the_fun = 0xFFF; double the_trouble = 0x666;}

class C : B
{
  int a = 12; int b = 13; int c = 14;

  final auto toArray()
  {
    int[6] arr;
    arr = [a, b, c, cast(int) more, cast(int) the_fun, cast(int) the_trouble];
    return arr;
  }

}

immutable int[6] expectedData = [12, 13, 14, 0x123, 0xFFF, 0x666];

auto testLayout()
{
    auto c = new C();
    // currently default ctors don't work
    // therefore we need to do this by hand
    c.a = 12; c.b = 13; c.c = 14;
    c.more = 0x123; c.the_fun = 0xFFF; c.the_trouble = 0x666;

    return c.toArray;
}
static immutable returnValue = testLayout();
pragma(msg, testLayout());
pragma(msg, expectedData);
static assert ( returnValue == expectedData );
