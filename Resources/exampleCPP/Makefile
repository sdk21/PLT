CC = gcc
CXX = g++

INCLUDES = -I/usr/include/eigen3/
CFLAGS = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

FILES = tensorProduct.cpp

.PHONY: default
default: 1 2 3 dj

1:
	$(CXX) prob1.cpp $(FILES) $(CXXFLAGS) -o Prob1

2:
	$(CXX) prob2.cpp $(FILES) $(CXXFLAGS) -o Prob2

3:
	$(CXX) prob3.cpp $(FILES) $(CXXFLAGS) -o Prob3

dj:
	$(CXX) deutschJozsa.cpp $(FILES) $(CXXFLAGS) -o DeutschJozsa

tp:
	$(CXX) tensorProduct.cpp $(CXXFLAGS) -o TPTest



.PHONY: clean
clean: 
	rm Prob1 Prob2 Prob3
