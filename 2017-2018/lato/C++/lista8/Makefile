COMPILER = g++
FLAGS = -Werror -Wall -g -std=c++17
TARGET = rational.exe

SRC = main.cpp rational.cpp

make:
	$(COMPILER) -o $(TARGET) $(FLAGS) $(SRC)
clean:
	$(RM) *.o $(TARGET)

.PHONY: depend clean