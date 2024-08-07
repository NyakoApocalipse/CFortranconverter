program sstringtest
	!声明字符串的方法
	integer :: i
	character(len=10) a !Fortran 90 添加
	character(10) b !Fortran 77
	character*20 c !Fortran 77
	character*(20) d !Fortran 77
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	character(len=20) e
	character(len=20) sstring
	character(len=6) first
	character(len=10) second
	character(len=20) add
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!字符串赋值
	a="Hello" !Fortran 90 可用双引号来封装字符串
	b='Hello' !Fortran 77 只能用单引号来封装字符串
	c="That's right." !用双引号封装字符串时，可以在字符串中任意使用单引号
	!d='That''s right.' !用单引号封转字符串时，输出单引号时要连续用两个单引号
	!e="That's ""right""."!用双引号来封装时，输出双引号也要连续使用两个双引号
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	sstring="Good morning."
	write(*,*) sstring
	sstring(6)="evening." !重新设置从第6个字符之后的字符串
	write(*,*) sstring
	sstring(1:2)="Go" !字符串最前面两个字符变为”Go“
	sstring(13:13)="!" !字符串的第13个字符变为“！”
	first="Happy "
	second="Birthday"
	add=first//second !经过两个连续的除号可以连接两个字符串
	write(*,*) add
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!一些常用函数
	!char(num) !返回计算机所使用的字符表上，数值num所代表的字符
	!ichar(char) !返回所输入的char字符在计算机所使用的字符表中所代表的编号，返回值是整数类型
	!len(sstring) !返回输入字符串的声明长度，返回值是整数类型
	!len_trim(sstring) !返回字符串去除尾端空格后的实际内容长度
	!index(sstring, key) !所输入的sstring可key都是字符串。这个函数会返回key这个“子字符串”在”母字符串“sstring中第一次出现的位置
	!trim(sstring) !返回把sstring字符串尾端多余空格清除后的字符串
	
end