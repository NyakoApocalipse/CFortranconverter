#!/bin/bash

find_for90std(){
  for f in $*; do
    sed -i '' 's/\(\.\.\/for90std\/for90std\.h\)/..\/\1/g' read.cpp
  done
}
filter_cost_time(){
  for f in $*; do
      if grep -q "Cost time:" $f; then
          sed -i '' '1d' $f
      else
          echo "cost time not found in ${f}"
      fi
  done
}
cd ../../build && make -j 12 && cd ../demos/forstdio
../../bin/CFortranTranslator -fF ./read.f90 > read.cpp
filter_cost_time read.cpp
find_for90std read.cpp

g++  read.cpp -Wall -DPOSIX -g -O0 -fpermissive -fPIC -std=c++17 -o a.out && ./a.out && rm a.out

