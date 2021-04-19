/**
 * The fiber module provides OS-indepedent lightweight threads aka fibers.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/fiber.d)
 */

module core.thread.myFiber;

version(LDC)
{
   import dmd.myFiber_ldc;
   mixin(module_string);
}
else
{
    import dmd.myFiber_dmd;
    mixin(module_string);
}
