COMPILER = g++
FLAGS = -std=c++14 -g -Wall -Wextra

TARGET = program
CPPS = line.cpp
OBJ = line.o

make:
	$(COMPILER) main.cpp $(CPPS) $(FLAGS) -o $(TARGET)
	$(RM) main.o
	$(RM) $(OBJ)
clean:
	$(RM) $(TARGET)
.PHONY: depend clean