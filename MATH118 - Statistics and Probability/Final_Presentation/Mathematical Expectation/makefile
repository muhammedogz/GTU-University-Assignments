CC = g++
CFLAGS =
HEADER = Expectation.hpp
SRC = Expectation.cpp
TEST = main.cpp

muo: $(TEST) $(SRC) $(HEADER) 
	$(CC) $(TEST) $(SRC) $(CFLAGS) -o output
	./output

debug: $(TEST) $(SRC) $(HEADER)
	$(CC) $(TEST) $(SRC) -g -o output
	gdb ./output

memory:
	valgrind ./output