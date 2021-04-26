CC = g++
CFLAGS =-Wextra -Wall
OBJS = main.cpp Covid.cpp

muo: main.cpp Covid.cpp Covid.h 
	$(CC) $(OBJS) $(CFLAGS) -o output
	./output > Covid_Output.csv

debug:
	$(CC) $(OBJS) $(CFLAGS) -g -o output
	gdb ./output

memory:
	valgrind ./output