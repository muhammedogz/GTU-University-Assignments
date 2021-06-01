CC = g++
CFLAGS =
OBJS = main.cpp Manufacture.cpp

muo: main.cpp Manufacture.cpp Manufacture.h 
	$(CC) $(OBJS) $(CFLAGS) -o output
	./output

debug:main.cpp Manufacture.cpp Manufacture.h 
	$(CC) $(OBJS) $(CFLAGS) -g -o output
	gdb ./output

memory:
	valgrind ./output