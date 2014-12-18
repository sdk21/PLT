#!/bin/sh

./qlc -g $1
g++ -w  ${1%.ql}.cpp -I./includes/headers -L./includes/libs -lqlang


