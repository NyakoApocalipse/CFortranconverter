#include "../for90std/for90std.h" 
#define USE_FORARRAY 
int main()
{
    forstring a = "          ";
    forstring b = "          ";
    a = SS("Hello");
    b = SS("Hello!");

    forwritefree(stdout, a);
    printf("\n");
    forwritefree(stdout, b);
    printf("\n");

    int tt = 104;
    forstring c = for_to_string(tt);
    forwritefree(stdout, c);
    printf("\n");

    forstring d = a + b;
    forwritefree(stdout, d);
    printf("\n");
    forwritefree(stdout, forlen(a));
    forwritefree(stdout, forlen(b));
    forwritefree(stdout, forlen(c));
    forwritefree(stdout, forlen(d));
    printf("\n");



    return 0;
}