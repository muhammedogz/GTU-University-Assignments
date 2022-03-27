CC = gcc
CFLAGS =-Wextra -Wall
HEADER = frobenius.h
SRC = main.c frobenius.c
HELPER = helper
HW = hw2
OUT = processP

muo: $(SRC) $(HEADER)
	$(CC) $(HELPER).c $(CFLAGS) -o $(HELPER)
	$(CC) $(SRC) $(CFLAGS) -o $(OUT)
	

debug: $(SRC) $(HEADER)
	$(CC) $(SRC) $(CFLAGS) -g -o $(OUT)
	
memory:
	valgrind ./$(OUT)