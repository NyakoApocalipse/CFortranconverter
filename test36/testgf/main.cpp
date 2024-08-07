#include <iostream>

extern "C" {
  void read_from_file_(char *filename, double *value);
  void write_to_file_(char *filename, double *value);
}
 
int main() {
  const char* read_filename = "input.txt";
  const char* write_filename = "output.txt";
  double value;
 
  // 调用Fortran的读取子程序
  std::cout<<"dump in here1"<<std::endl;
  read_from_file_(const_cast<char*>(read_filename), &value);
  std::cout<<"dump in here2"<<std::endl;
  // 调用Fortran的写入子程序
  write_to_file_(const_cast<char*>(write_filename), &value);
 
  return 0;
}