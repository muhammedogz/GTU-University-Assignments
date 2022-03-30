#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "processR.h"

extern char **environ;

int main(int argc, char *argv[])
{
  // Initializing siggnal action for SIGINT signal.
  struct sigaction sig_int_;
  memset(&sig_int_, 0, sizeof(sig_int_)); // Initializing
  // sig_int_.sa_handler = sg_signalHandler;        // Setting the handler function.
  sig_int_.sa_flags = 0;              // No flag is set.
  sigaction(SIGINT, &sig_int_, NULL); // Setting the signal.

  char *filePath = argv[3]; // Output file path
  int fileDesc;             // Directory stream file descriptor for file writing
  struct flock lock;        // Lock structure of the file.

  // Opening file in write mode
  if ((fileDesc = open(filePath, O_WRONLY | O_APPEND, S_IWGRP)) == -1)
  {
    perror("Error while opening file to write.\n");
    exit(EXIT_FAILURE);
  }

  // Locking
  memset(&lock, 0, sizeof(lock)); // Initing structure of lock to 0.
  lock.l_type = F_WRLCK;          // F_WRLCK: Field of the structure of type flock for write lock.
  if (fcntl(fileDesc, F_SETLKW, &lock) == -1)
  { // putting write lock on file.
    // F_SETLKW: If a signal is caught while waiting, then the call is interrupted and (after signal handler returned) returns immediately.
    perror("Error while locking fcntl(F_SETLK mode).\n");
    exit(EXIT_FAILURE);
  }
  // print arguments
  printf("argc: %d\n", argc);
  for (int i = 0; i < argc; i++)
  {
    printf("argv[%d]: %s\n", i, argv[i]);
  }

  // Unlocking
  lock.l_type = F_UNLCK;
  if (fcntl(fileDesc, F_SETLKW, &lock) == -1)
  {
    perror("Error while unlocking with fcntl(F_SETLKW)");
    exit(EXIT_FAILURE);
  }

  // Closing file
  if (close(fileDesc) == -1)
  {
    perror("Error while closing the file.");
    exit(EXIT_FAILURE);
  }

  return 0;
}

void print_process_info(char *process_name)
{
  write(STDOUT_FILENO, "CREATED R_", 10);
  write(STDOUT_FILENO, process_name, strlen(process_name));
  write(STDOUT_FILENO, " with ", 7);
  write(STDOUT_FILENO, environ[0], strlen(environ[0]));
  write(STDOUT_FILENO, ",", 1);
  write(STDOUT_FILENO, environ[1], strlen(environ[1]));
  write(STDOUT_FILENO, ",", 1);
  write(STDOUT_FILENO, environ[2], strlen(environ[2]));
  write(STDOUT_FILENO, ",...,", 5);
  write(STDOUT_FILENO, environ[9], strlen(environ[9]));
  write(STDOUT_FILENO, "\n", 1);
}