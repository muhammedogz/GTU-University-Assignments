CC = gcc
CFLAGS =-Wextra -Wall -lrt

CLEAR = 

NAMED_SRC = main_named.c src/named.c
NAMED_HEADER = include/named.h
NAMED_OUT = hw3named
NAMED_ARGS = -i data/ingredients.txt -n domates

UNNAMED_SRC = main_unnamed.c src/unnamed.c
UNNAMED_HEADER = include/unnamed.h
UNNAMED_OUT = hw3unnamed
UNNAMED_ARGS = -i data/ingredients.txt

COMMON_SRC = src/common.c
COMMON_HEADER = include/common.h

.DEFAULT_GOAL = all # default goal, compile both client and server when typing make

named: $(NAMED_HEADER) $(NAMED_SRC) $(COMMON_SRC)
	$(CC) $(NAMED_SRC) $(COMMON_SRC) -o $(NAMED_OUT) $(CFLAGS)

unnamed: $(UNNAMED_HEADER) $(UNNAMED_SRC) $(COMMON_SRC)
	$(CC) $(UNNAMED_SRC) $(COMMON_SRC) -o $(UNNAMED_OUT) $(CFLAGS)

all: named unnamed

run_named:
	./$(NAMED_OUT) $(NAMED_ARGS)

run_unnamed:
	./$(UNNAMED_OUT) $(UNNAMED_ARGS)
	
memory_named:
	valgrind --leak-check=full --show-leak-kinds=all ./$(NAMED_OUT) $(NAMED_ARGS)

memory_unnamed:
	valgrind --leak-check=full --show-leak-kinds=all ./$(UNNAMED_OUT) $(UNNAMED_ARGS)

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
	ls /dev/shm -a
	
controling_terminal:
	ps -xj | grep serverY

# First send a SIGUSR1 signal to the server, then send a SIGUSR2 signal to the server
# After send 9 to make sure the server is killed
kill_named:
	killall $(NAMED_OUT) -2

kill_unnamed:
	killall $(UNNAMED_OUT) -2