main:
	gcc tweetsGenerator.c -o tweetsGenerator -Wall -Wextra -Wvla -std=c99
	./tweetsGenerator 579543 10 justdoit_tweets.txt

debug:
	gcc tweetsGenerator.c -o -g tweetsGenerator -Wall -Wextra -Wvla -std=c99
	gdb ./tweetsGenerator

memory:
	valgrind ./tweetsGenerator 579543 10 justdoit_tweets.txt