CC = gcc
CXX = g++

INCLUDES = -I/usr/include/eigen3/
CFLAGS = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

.PHONY: default
default: 1 2 3

1:
	$(CXX) prob1.cpp $(CXXFLAGS) -o Prob1

2:
	$(CXX) prob2.cpp $(CXXFLAGS) -o Prob2

3:
	$(CXX) prob3.cpp $(CXXFLAGS) -o Prob3


.PHONY: clean
clean: 
	rm Prob1 Prob2 Prob3
