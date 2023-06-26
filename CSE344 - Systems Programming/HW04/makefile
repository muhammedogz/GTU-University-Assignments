CC = gcc
CFLAGS =-Wextra -Wall -lrt -lpthread -pthread

UTILS_SRC = src/utils.c
UTILS_HEADER = include/utils.h

MAIN = main.c

# You can change args value due to more complex exmaple
SIMPLE = -C 6 -N 4 -F data/simple.txt
COMPLEX = -N 80 -C 80 -F data/complex.txt

# Change this
ARGS = $(SIMPLE)

OUT = hw4

CLEAN = $(OUT)

.DEFAULT_GOAL = all # default goal, compile both client and server when typing make

muo: $(UTILS_SRC) $(UTILS_HEADER) $(MAIN)
	$(CC) $(CFLAGS) $(UTILS_SRC) $(MAIN) -o $(OUT) 

all: muo

run:
	./$(OUT) $(ARGS)

memory:
	valgrind --leak-check=full --show-leak-kinds=all ./$(OUT) $(ARGS)


clean:
	rm $(CLEAN)

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
