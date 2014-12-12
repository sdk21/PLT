CC = g++
CFLAGS :=
LDFLAGS :=
SS = SemanticSuccess
SF = SemanticFailures
AL = Algorithms
SS_SRC := $(wildcard $(SS)/*.cpp)
SS_OBJ := $(addprefix $(SS)/,$(notdir $(SS_SRC:.cpp=.o)))
SF_SRC := $(wildcard $(SF)/*.cpp)
SF_OBJ := $(addprefix $(SF)/,$(notdir $(SF_SRC:.cpp=.o)))
AL_SRC := $(wildcard $(AL)/*.cpp)
AL_OBJ := $(addprefix $(AL)/,$(notdir $(AL_SRC:.cpp=.o)))

all: $(SS) $(SF) $(AL)

$(SS): $(SS_OBJ)
	$(CC) -o $@ $^

$(SS)/%.o: $(SS)/%.cpp
	$(CC) $(CC_FLAGS) -c -o $@ $<

$(SF): $(SF_OBJ)
	$(CC) -o $@ $^

$(SF)/%.o: $(SF)/%.cpp
	$(CC) $(CC_FLAGS) -c -o $@ $<

$(AL): $(AL_OBJ)
	$(CC) -o $@ $^

$(AL)/%.o: $(AL)/%.cpp
	$(CC) $(CC_FLAGS) -c -o $@ $<

.PHONY : clean
clean:

