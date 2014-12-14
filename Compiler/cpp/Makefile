CC = gcc
CXX = g++

#../../includes should direct to the includes directory
#located in the home directory of the qlang project
INC = ./ ../includes/libs
INCLUDES =$(INC:%=-I%)
CFLAGS = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

#LDFLAGS = -g -L../readHST 
#LDLIBS = -lpthread

.PHONY: default
default: libqlang.a
	mv libqlang.a ../includes/libs
	cp qlang.hpp ../includes/headers/qlang


libqlang.a: qlang.o
	ar rc libqlang.a qlang.o
	ranlib libqlang.a

qlang.o: qlang.cpp
	$(CXX) -c qlang.cpp $(CFLAGS)

clean:
	rm *.o *.a

