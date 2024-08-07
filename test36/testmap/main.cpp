#include "mm.h"
using namespace std;
int main()
{
    int a = 1;
    char* file_name = "test.txt";
    map_init(a,b);
    char[1024] buffer;
    std::cout<<"In main file"<<std::endl;
    std::cout<<(reclnum.find(a) != reclnum.end())<<std::endl;
    reclnum[a].read(buffer, 4);
    std::cout<<buffer<<std::endl;
    print_map(a);
    cout<<endl<<"in main"<<endl;
    cout<<(reclnum.find(a) != reclnum.end())<<endl;
    return 0;
}