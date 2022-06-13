CC = gcc
CFLAGS =-Wextra -Wall -lrt -lpthread -pthread

COMMON_SRC = src/common.c
COMMON_HEADER = include/common.h

CLIENT_SRC = main_client.c src/client.c
CLIENT_HEADER = include/client.h
CLIENT_OUT = client

SERVERY_SRC = main_server.c src/server.c
SERVERY_HEADER = include/server.h
SERVERY_OUT = server

SERVANT_SRC = main_servant.c src/servant.c
SERVANT_HEADER = include/servant.h
SERVANT_OUT = servant

PORT = 11240

CLIENT_ARGS = -r data/requestFile -q $(PORT) -s 127.0.0.1
SERVER_ARGS = -p $(PORT) -t 11
SERVANT_ARGS = -d data/dataset -c 1-9 -r 127.0.0.1 -p $(PORT)

CLEAN = $(SERVANT_OUT) $(CLIENT_OUT) $(SERVERY_OUT)

.DEFAULT_GOAL = all # default goal

client: $(CLIENT_HEADER) $(CLIENT_SRC) $(COMMON_SRC)
	$(CC) $(CLIENT_SRC) $(COMMON_SRC) -o $(CLIENT_OUT) $(CFLAGS)

server: $(SERVERY_HEADER) $(SERVERY_SRC) $(COMMON_SRC)
	$(CC) $(SERVERY_SRC) $(COMMON_SRC) -o $(SERVERY_OUT) $(CFLAGS)

servant: $(SERVANT_HEADER) $(SERVANT_SRC) $(COMMON_SRC)
	$(CC) $(SERVANT_SRC) $(COMMON_SRC) -o $(SERVANT_OUT) $(CFLAGS)

all: client server servant

run_client:
	./$(CLIENT_OUT) $(CLIENT_ARGS)

run_server:
	./$(SERVERY_OUT) $(SERVER_ARGS)

run_servant:
	./$(SERVANT_OUT) $(SERVANT_ARGS)

clean:
	rm $(CLEAN)

# Check memory leaks with valgrind
valgrind_client:
	valgrind --leak-check=full --show-leak-kinds=all ./$(CLIENT_OUT) $(CLIENT_ARGS)

valgrind_server:
	valgrind --leak-check=full --show-leak-kinds=all ./$(SERVERY_OUT) $(SERVER_ARGS)

valgrind_servant:
	valgrind --leak-check=full --show-leak-kinds=all ./$(SERVANT_OUT) $(SERVANT_ARGS)

# show all zombie processes
# if you want to run this command in your terminal, you can use this
# ps aux | awk '"[Zz]" ~ $8 { printf("%s, PID = %d\n", $8, $2); }'
zombies:
	ps aux | awk '"[Zz]" ~ $$8 { printf("%s, PID = %d\n", $$8, $$2); }'

# Show all not freed shared memroy, semaphore, and mutexes
memory:
	ipcs
	ls /dev/shm -a
	
# check controlling terminal (for midterm project but I dont removed it)
controling_terminal:
	ps -xj | grep serverY

# Send sigint to process
kill_client: 
	killall $(CLIENT_OUT) -2

kill_server:
	killall $(SERVERY_OUT) -2

kill_servant:
	killall $(SERVANT_OUT) -2
