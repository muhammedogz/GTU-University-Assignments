main:
	gcc tweetsGenerator.c -o tweetsGenerator -Wall -Wextra -Wvla -std=c99
	./tweetsGenerator 26 10 justdoit_tweets.txt

debug:
	gcc tweetsGenerator.c -o -g tweetsGenerator -Wall -Wextra -Wvla -std=c99
	gdb ./tweetsGenerator 579543 10 justdoit_tweets.txt

memory:
#--track-origins=yes -s
	valgrind --leak-check=full  ./tweetsGenerator 579543 10 justdoit_tweets.txt 100