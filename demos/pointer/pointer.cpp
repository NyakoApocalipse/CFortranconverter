/**********************************************************************/
/* File:                                                              */
/* Author:                                                            */
/* This codes is generated by CFortranTranslator                      */
/* CFortranTranslator is published under GPL license                  */
/* refer to https://github.com/CalvinNeo/CFortranTranslator/ for more */
/**********************************************************************/
#include "../../for90std/for90std.h" 
#define USE_FORARRAY 
int main()
{
	int i = 0;
	int * ipnodet = nullptr;
	int j = 0;
	farray<int> nodet {};
	/*!ipnodet是指针,其值是指针对象的地址；nodet(3)是指针对象，一个13的数组*/
	ipnodet = ((int *)malloc(12));
	nodet.parr = ipnodet;
	nodet.reset_array({{1,3},{1,(12 / (1 * 3))}});;
	/*!6（一个整数）为内存分配数，返回值为分配的内存块起始位置的地址*/
	for(i = 1; i <= 3; i += 1){
		for(j = 1; j <= 4; j += 1){
			nodet(INOUT(i), INOUT(j)) = i + j;
			
		}
		nop();
		
	}
	nop();
	forwritefree(stdout, ipnodet, nodet(INOUT(2), INOUT(3)));
	
	return 0;
}