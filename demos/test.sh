#!/bin/bash

filter_cost_time(){
  for f in $*; do
      if grep -q "Cost time:" $f; then
          sed -i '' '1d' $f
      else
          echo "cost time not found in ${f}"
      fi
  done
}
cd ../build && make -j 12 && cd ../demos
../bin/CFortranTranslator -fF ../demos/stest1.f90 > ../demos/stest1.cpp
../bin/CFortranTranslator -fF ../demos/subr.f90 > ../demos/ma.h
filter_cost_time ../demos/subr.cpp
filter_cost_time ../demos/stest1.cpp
g++  ../demos/stest1.cpp -Wall -DPOSIX -g -O3 -fpermissive -fPIC -std=c++17 -o ../demos/a.out && ../demos/a.out && rm ../demos/a.out

