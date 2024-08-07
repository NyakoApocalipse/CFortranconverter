/**********************************************************************/
/* File:                                                              */
/* Author:                                                            */
/* This codes is generated by CFortranTranslator                      */
/* CFortranTranslator is published under GPL license                  */
/* refer to https://github.com/CalvinNeo/CFortranTranslator/ for more */
/**********************************************************************/
#include "../for90std/for90std.h" 
#define USE_FORARRAY 
int main()
{
	forstring dataa = "    ";
	bool eof;
	int i = 0;
	int iostatt = 0;
	
	
	foropenfile(10, None, None, SS("temp.txt"), SS("old"), SS("sequential"), None, 4, None, SS("asis"), SS("readwrite"), None, None);
	/*! 打开文件  */
	if (iostatt != 0) {
		forprintfree(SS("Error opening file"));
		stop();;
		
	}
	
	
	for(i = 1; i <= 5; i += 1){
		forreadfree(10, get_file(10), &dataa);
		/*! 从文件中读取数据  */
		if (iostatt != 0) {
			forprintfree(SS("Error reading from file"));
			break;
			
		}
		forprintfree(SS("Read data:"), dataa);
		forprintfree(SS("BREAK!!\n\n"));
		
	}
	nop();
	
	
	forwritefree(10, get_file(10),  SS("1234"));
	/*! 在文件末尾写入数据  */
	forwritefree(10, get_file(10),  SS("5678"));
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	forclosefile(10, None, None);
	/*! 为了读取刚才写入的数据，需要将文件指针重置到文件开始  */
	/*! 但由于我们想在末尾继续，实际上这里应该重新打开文件或以追加模式打开  */
	/*! 不过为了简单起见，这里我们假设直接读取接下来的数据（这在实际操作中可能不是最佳做法）  */
	/*! 尝试读取并打印新写入的数据（注意：在实际应用中，可能需要先关闭再重新打开文件，或者调整文件指针）  */
	/*! rewind(10) ! 将文件指针移回文件开头，仅为了示例，实际上应该重新打开文件或寻求其他方式读取末尾数据  */
	/*! ! 如果文件很大，不想从头开始读取，应该使用其他方法来定位到文件末尾  */
	/*! do while (.not. eof(10))  */
	/*!     read(10, *, iostat=iostat) data  */
	/*!     if (iostat /= 0) then  */
	/*!         if (iostat == -1) then  */
	/*!             eof = .true.  */
	/*!         else  */
	/*!             print *, 'Error reading from file during rewind'  */
	/*!             exit  */
	/*!         end if  */
	/*!     else  */
	/*!         print *, 'Data after rewind:', data  */
	/*!     end if  */
	/*! end do  */
	/*! 关闭文件  */
	
	
	
	
	return 0;
}
