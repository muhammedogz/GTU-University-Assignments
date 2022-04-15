CC = gcc
CFLAGS =-Wextra -Wall -lm

COMMON_SRC = src/common.c
COMMON_HEADER = include/common.h

CLIENT_SRC = main_client.c src/client.c
CLIENT_HEADER = include/client.h
CLIENT_OUT = client

CLIENT_ARGS = -s domates -o data/data.csv


client: $(CLIENT_HEADER) $(CLIENT_SRC) $(COMMON_SRC)
	$(CC) $(CFLAGS) $(CLIENT_SRC) $(COMMON_SRC) -o $(CLIENT_OUT)

run_client:
	./$(CLIENT_OUT) $(CLIENT_ARGS)
	

debug: 
	$(CC) $(MAIN) $(SRC) $(CFLAGS) -g -o $(CLIENT_OUT)
	
memory:
	valgrind ./$(CLIENT_OUT) $(CLIENT_ARGS)