CC = gcc
CFLAGS =-Wextra -Wall
HEADER = replacer.h
SRC = main.c frobenius.c
HELPER = helper
HW = hw2

muo: $(SRC) $(HEADER)
	$(CC) $(HELPER).c $(CFLAGS) -o $(HELPER)
	$(CC) $(SRC) $(CFLAGS) -o $(HW)
	

debug: $(SRC) $(HEADER)
	$(CC) $(SRC) $(CFLAGS) -g -o $(HW)
	
memory:
	valgrind ./$(HW)