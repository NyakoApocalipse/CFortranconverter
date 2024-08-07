#ifndef MM_H
#define MM_H
#include <map>
#include <iostream>
#include <fstream>

std::map<int, std::fstream&> reclnum;

void map_init(int a, const char* file_name)
{
    std::fstream file;
    reclnum[a] = file;
    file.open(file_name, std::ios::binary);
    file.write(file_name, 5);
    return;
}

void print_map(int a)
{
    char[1024] buffer;
    std::cout<<"In the mm file"<<std::endl;
    std::cout<<(reclnum.find(a) != reclnum.end())<<std::endl;
    reclnum[a].read(buffer, 4);
    std::cout<<buffer<<std::endl;
    // std::cout<<reclnum[a]<<std::endl;
    reclnum[a].close();
    reclnum.erase(a);
    return;
}
#endif