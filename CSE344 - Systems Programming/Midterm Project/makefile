CC = gcc
CFLAGS =-Wextra -Wall -lm
HEADER = include/client.h include/common.h
SRC = src/client.c src/common.c
MAIN = main.c
OUT = client
HW = midterm

muo: $(SRC) $(HEADER) $(MAIN)
	$(CC) $(MAIN) $(SRC) $(CFLAGS) -o $(OUT)
	

debug: $(SRC) $(HEADER) $(MAIN)
	$(CC) $(MAIN) $(SRC) $(CFLAGS) -g -o $(OUT)
	
memory:
	valgrind ./$(OUT)