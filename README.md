# Fortran to C++ Converter 1.0.0 
For project introduction and steps to build and install, refer to [CFortranTranslator](https://github.com/CalvinNeo/CFortranTranslator).

In this work, we try to add more features in fortran to the work of [YHN](https://github.com/YHN-ice/CFortranTranslator):

## 2024.07.24
Fix forstring impl, add imcompleted impl of file io with arg recl, work of it in the future will be push in branch dev until it is completed and verified.

## 2024.07.19
We decide that this work won't support ENTRY keyword in fortran & F90. If needed, please split the code manually to replace the functions of ENTRY.

## 2024.07.07
Add fortran-style string(or we say CHARACTER) implementation in C++, now we can use len() and other functions in fortran to operate strings.
