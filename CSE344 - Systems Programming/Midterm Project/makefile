CC = gcc
CFLAGS =-Wextra -Wall

COMMON_SRC = src/common.c
COMMON_HEADER = include/common.h

CLIENT_SRC = main_client.c src/client.c
CLIENT_HEADER = include/client.h
CLIENT_OUT = client

SERVERY_SRC = main_server.c src/serverY.c
SERVERY_HEADER = include/serverY.h
SERVERY_OUT = serverY

CLIENT_ARGS = -s domates -o data/data.csv
SERVERY_ARGS = -s domates -o data/log.txt -p 4 -r 4 -t 4


client: $(CLIENT_HEADER) $(CLIENT_SRC) $(COMMON_SRC)
	$(CC) $(CFLAGS) $(CLIENT_SRC) $(COMMON_SRC) -o $(CLIENT_OUT)

server: $(SERVERY_HEADER) $(SERVERY_SRC) $(COMMON_SRC)
	$(CC) $(CFLAGS) $(SERVERY_SRC) $(COMMON_SRC) -o $(SERVERY_OUT)

run_client:
	./$(CLIENT_OUT) $(CLIENT_ARGS)

run_server:
	./$(SERVERY_OUT) $(SERVERY_ARGS)
	

debug: 
	$(CC) $(MAIN) $(SRC) $(CFLAGS) -g -o $(CLIENT_OUT)
	
memory:
	valgrind ./$(CLIENT_OUT) $(CLIENT_ARGS)