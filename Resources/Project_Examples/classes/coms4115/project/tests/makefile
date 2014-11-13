DEPTH = ..

TESTS = \
    hello \
    addmat \
    submat \
    scalmul \
    mulmat \
    elemacc \
    vectacc \
    iftest \
    ifelse \
    matsize \
    matpow \
    badpow \
    rowbound \
    colbound \
    redim \
    trans \
    tutorial \
    vel

all : $(TESTS)

$(TESTS) :
    $(DEPTH)\lame < $@.lam > $@.cpp
    cl -nologo -EHsc -I$(DEPTH) $@.cpp
    $@ > output\$@.out
