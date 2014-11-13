#! /bin/bash

find . -name *.mcb -exec ./generate_output.pl '{}' \;
