#include "../for90std/for90std.h"
#define USE_FORARRAY
int main()
{
    farray<string> mat_gob_long{ {1}, {6} };
    mat_gob_long(INOUT(6)) = SS("f");
    mat_gob_long(INOUT(5)) = SS("e");
    mat_gob_long(INOUT(4)) = SS("d");
    mat_gob_long(INOUT(3)) = SS("c");
    mat_gob_long(INOUT(2)) = SS("b");
    mat_gob_long(INOUT(1)) = SS("a");
    forwritefree(stdout, mat_gob_long);

    return 0;
}