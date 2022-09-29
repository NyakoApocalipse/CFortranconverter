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
../bin/CFortranTranslator -fF ../demos/neg.f90 > ../demos/neg.cpp
filter_cost_time ../demos/neg.cpp
g++  ../demos/neg.cpp -Wall -DPOSIX -g -O3 -fpermissive -fPIC -std=c++17 -o ../demos/a.out && ../demos/a.out && rm ../demos/a.out

