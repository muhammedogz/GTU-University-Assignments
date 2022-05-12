CC = gcc
CFLAGS =-Wextra -Wall -lrt -lpthread

UTILS_SRC = src/utils.c
UTILS_HEADER = include/utils.h

MAIN = main.c
ARGS = -f -a -b
OUT = hw4

CLEAR = $(OUT)

.DEFAULT_GOAL = all # default goal, compile both client and server when typing make

muo: $(UTILS_SRC) $(UTILS_HEADER) $(MAIN)
	$(CC) $(CFLAGS) $(UTILS_SRC) $(MAIN) -o $(OUT) 

all: muo

run:
	./$(OUT) $(ARGS)

clear:
	rm $(CLEAR)

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

kill:
	killall $(OUT) -2
