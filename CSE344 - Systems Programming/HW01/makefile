CC = gcc
CFLAGS =-Wextra -Wall
HEADER = replacer.h
SRC = main.c replacer.c

muo: $(SRC) $(HEADER)
	$(CC) $(SRC) $(CFLAGS) -o hw1
	

debug: $(SRC) $(HEADER)
	$(CC) $(SRC) $(CFLAGS) -g -o hw1
	
memory:
	valgrind ./hw1