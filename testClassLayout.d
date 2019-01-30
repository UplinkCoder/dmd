
alias twice = long; alias once = int;
class B {once more = 0x123; twice the_fun = 0xFFF;}

class C : B
{
  int a = 12; int b = 13; int c = 14;

  final int[5] toArray()
  {
    int[5] arr;
    arr = [a, b, c, cast(int) more, cast(int) the_fun];
    return arr;
  }

}

immutable int[5] expectedData = [12, 13, 14, 0x123, 0xFFF];

auto testLayout()
{
    auto c = new C();
    // currently default ctors don't work
    // therefore we need to do this by hand
    c.a = 12; c.b = 13; c.c = 14;
    c.the_fun = 0xFFF;
    c.more = 0x123;

    return c.toArray;
}
static immutable returnValue = testLayout();
pragma(msg, testLayout());
pragma(msg, expectedData);
static assert ( returnValue == expectedData );
