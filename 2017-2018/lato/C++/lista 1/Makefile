COMPILER = g++
FLAGS = -std=c++11 -g -Wall -Wextra

TARGET = prime-factors

make:
	$(COMPILER) main.cpp $(FLAGS) -o $(TARGET)
	$(RM) main.o
clean:
	$(RM) $(TARGET)
	$(RM) $(OBJ)
.PHONY: depend clean