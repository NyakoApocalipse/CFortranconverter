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
cd ../build && make -j 12
../bin/CFortranTranslator -fF ../demos/4.for > ../demos/4.cpp

filter_cost_time ../demos/4.cpp
g++  ../demos/4.cpp -Wall -DPOSIX -g -O3 -fpermissive -fPIC -std=c++17 -o ../demos/a.out && ../demos/a.out && rm ../demos/a.out

