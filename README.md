# Fortran to C++ Converter 1.0.0 
For project introduction and steps to build and install, refer to [CFortranTranslator](https://github.com/CalvinNeo/CFortranTranslator).

In this work, we try to add more features in fortran to the work of [YHN](https://github.com/YHN-ice/CFortranTranslator):

## 2024.08.14
We temporarily stop improving this project for some reason. 

There are something other developers need to know: we add some common words like "result"\"data" and so on as keywords to test some functions of this project. We didn't remove these changes until the newest version, so if you use this project and find some error messages like "Unexpected 'YY_RESULT' ", this is the reason. If you want to remove this change or what, please check in src/grammar/ folder for more hints. 

## 2024.08.07
1. Add implement of assign_forslice() in "farray.h" . 
2. We decided not to implement param rec in direct read/write file operations. If anyone wants to implement it, please refer to advices below:

2-1 Use the way like reclnum or filenos to add a global flag mapped with file no. .
2-2 Change it in foropenfile(), so we can use the flag later. Default is "Sequential".
2-3 Add new implements of read/write operations with param rec, remind that without param recl, rec is illegal.
2-4 Update other functions to fit your codes, if needed, update code generation part.


## 2024.08.07
We implement file io with param recl and other params. But we didn't implement the file io while ACCESS="direct" and param recl is determined. In this case, if io with param rec determined, the file pointer should move rec*recl units before any other operation. We will implement it and function assign_forslice() which is in "farray.h" before Aug 9th. The latter function is used to assign farray itself instead of make a deep copy slice of any farray object.



## 2024.08.03
Found that the behaviours of file handling in FORTRAN are very different from C++, such as:
1. recl behaviours, we are struggling in conclude this kind of behaviours. Now we know that with recl: 
1.1 fortran program will read a recl long data each LINE, ignoring '\n' space '\t' and other special characters. 
1.2 And will try to write a recl long data each LINE, with an auto space ahead.

Without recl, these actions are similar but with no restriction of data length.

Our job is to create a converter program, so we don't need to simulate the behaviours of FORTRAN in C++ completely, we just need to ensure the converted program can work correctly. So we want to implement a 'C++ style' FORTRAN file io first, then add the support of data file created by fortran program in the future.

## 2024.07.24
Fix forstring impl, add imcompleted impl of file io with arg recl, work of it in the future will be push in branch dev until it is completed and verified.

## 2024.07.19
We decide that this work won't support ENTRY keyword in fortran & F90. If needed, please split the code manually to replace the functions of ENTRY.

## 2024.07.07
Add fortran-style string(or we say CHARACTER) implementation in C++, now we can use len() and other functions in fortran to operate strings.
