CC = g++
CFLAGS =-Wextra -Wall -lrt -lpthread

HEADER = include/ThreadSafeSet.hpp include/Node.hpp
SRC = src/ThreadSafeSet.cpp
MAIN = main.cpp

OUT = main

all: $(MAIN) $(SRC) $(HEADER)
	$(CC) $(SRC) $(MAIN) $(CFLAGS) -o $(OUT)

run: all
	./$(OUT)

clean:
	rm -f $(OUT)

memory:
	valgrind --leak-check=full --show-leak-kinds=all ./$(OUT)

test_with_script:
	chmod +x test_client.sh
	./test_client.sh

# show all zombie processes
# if you want to run this command in your terminal, you can use this
# ps aux | awk '"[Zz]" ~ $8 { printf("%s, PID = %d\n", $8, $2); }'
zombies:
	ps aux | awk '"[Zz]" ~ $$8 { printf("%s, PID = %d\n", $$8, $$2); }'

kill:
	killall $(OUT) -2
