CC = gcc
CFLAGS =-Wextra -Wall -lrt

CLEAR = serverY client

COMMON_SRC = src/common.c
COMMON_HEADER = include/common.h

CLIENT_SRC = main_client.c src/client.c
CLIENT_HEADER = include/client.h
CLIENT_OUT = client

SERVERY_SRC = main_server.c src/serverY.c
SERVERY_HEADER = include/serverY.h
SERVERY_OUT = serverY

CLIENT_ARGS = -s serverFifo -o data/data.csv
SERVERY_ARGS = -s serverFifo -o data/log.txt -p 4 -r 4 -t 6

.DEFAULT_GOAL = all # default goal, compile both client and server when typing make

client: $(CLIENT_HEADER) $(CLIENT_SRC) $(COMMON_SRC)
	$(CC) $(CLIENT_SRC) $(COMMON_SRC) -o $(CLIENT_OUT) $(CFLAGS)

server: $(SERVERY_HEADER) $(SERVERY_SRC) $(COMMON_SRC)
	$(CC) $(SERVERY_SRC) $(COMMON_SRC) -o $(SERVERY_OUT) $(CFLAGS)

all: client server

run_client:
	./$(CLIENT_OUT) $(CLIENT_ARGS)

run_server:
	./$(SERVERY_OUT) $(SERVERY_ARGS)
	
memory_server:
	valgrind --leak-check=full --show-leak-kinds=all ./$(SERVERY_OUT) $(SERVERY_ARGS)

memory_client:
	valgrind --leak-check=full --show-leak-kinds=all ./$(CLIENT_OUT) $(CLIENT_ARGS)

clear:
	rm $(CLEAR)

test_with_script:
	chmod +x test_client.sh
	./test_client.sh

# show all zombie processes
# if you want to run this command in your terminal, you can use this
# ps aux | awk '"[Zz]" ~ $8 { printf("%s, PID = %d\n", $8, $2); }'
zombies:
	ps aux | awk '"[Zz]" ~ $$8 { printf("%s, PID = %d\n", $$8, $$2); }'

shared_mem_leak:
	ipcs
	
controling_terminal:
	ps -xj | grep serverY