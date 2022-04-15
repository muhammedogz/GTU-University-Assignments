CC = gcc
CFLAGS =-Wextra -Wall -lm
HEADER = include/client.h include/common.h
SRC = src/client.c src/common.c
MAIN = main.c
CLIENT_OUT = client
HW = midterm
CLIENT_ARGS = -s domates -o data/data.csv

muo: $(SRC) $(HEADER) $(MAIN)
	$(CC) $(MAIN) $(SRC) $(CFLAGS) -o $(CLIENT_OUT)
	./$(CLIENT_OUT) $(CLIENT_ARGS)

run_client:
	./$(CLIENT_OUT) $(CLIENT_ARGS)
	

debug: $(SRC) $(HEADER) $(MAIN)
	$(CC) $(MAIN) $(SRC) $(CFLAGS) -g -o $(CLIENT_OUT)
	
memory:
	valgrind ./$(CLIENT_OUT) $(CLIENT_ARGS)