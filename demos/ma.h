/**********************************************************************/
/* File:                                                              */
/* Author:                                                            */
/* This codes is generated by CFortranTranslator                      */
/* CFortranTranslator is published under GPL license                  */
/* refer to https://github.com/CalvinNeo/CFortranTranslator/ for more */
/**********************************************************************/
#include "../for90std/for90std.h" 
#define USE_FORARRAY 
void xx(double && a);
#ifndef ma_xx
#define ma_xx
void xx(double && a)
{
	forwritefree(stdout, a);
	
	return ;
}

#endif
#ifndef ma_car
#define ma_car
struct car
{
	double speed = 0.0;
	
};

#endif
#ifndef ma
#define ma
car car1;

#endif

