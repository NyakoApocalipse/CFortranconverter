cd ../build && make -j 12
../bin/CFortranTranslator -fF ../demos/3.for > ../demos/3.cpp
../bin/CFortranTranslator -fF ../demos/ma.for > ../demos/ma.h
g++  ../demos/3.cpp -Wall -DPOSIX -g -O3 -fpermissive -fPIC -std=c++17 -o ../demos/a.out && ../demos/a.out

# =>
# 1.000000