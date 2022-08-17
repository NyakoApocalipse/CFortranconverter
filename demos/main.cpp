#include "../for90std/for90std.h"
#define USE_FORARRAY

int main()
{
    farray<int> c {{1,1},{3,4},forreshape({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}, {3, 4})};

  forprintfree(c);
  return 0;
}
