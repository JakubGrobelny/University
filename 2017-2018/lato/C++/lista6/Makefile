CC = g++
FLAGS = -std=c++17 -Wall -Werror -g

TARGET = expressions

SRC = $(wildcard *.cpp)
OBJ = $(patsubst %.cpp, obj/%.o, $(SRC))

$(TARGET) : $(OBJ)
	$(CC) $(FLAGS) -o $(TARGET) $(OBJ)
obj/%.o : %.cpp
	@mkdir -p obj
	$(CC) $(FLAGS) -c $< -o $@
clean:
	$(RM) -rf obj
	$(RM) $(TARGET)

.PHONY: depend clean
