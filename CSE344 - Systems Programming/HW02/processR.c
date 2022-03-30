#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "processR.h"

static volatile sig_atomic_t sig_flag = 0;

// only hanlde SIGINT func
static void sig_handler(int signo)
{
  if (signo == SIGINT)
    sig_flag = 1;
}

extern char **environ;

int main(int argc, char *argv[])
{

  struct sigaction sig_int_action;
  memset(&sig_int_action, 0, sizeof(sig_int_action));
  sig_int_action.sa_handler = sig_handler;
  sig_int_action.sa_flags = 0;
  sigaction(SIGINT, &sig_int_action, NULL);

  char *filePath = argv[2];
  int fileDesc;
  struct flock lock;

  if ((fileDesc = open(filePath, O_WRONLY | O_APPEND, S_IWGRP)) < 0)
  {
    perror("OPEN ERROR\n");
    exit(EXIT_FAILURE);
  }

  memset(&lock, 0, sizeof(lock));
  lock.l_type = F_WRLCK;
  if (fcntl(fileDesc, F_SETLKW, &lock) == -1)
  {
    perror("Lock Error.\n");
    exit(EXIT_FAILURE);
  }

  // write environ[0] to file
  write(fileDesc, environ[0], strlen(environ[0]));
  write(fileDesc, "\n", 1);

  lock.l_type = F_UNLCK;
  if (fcntl(fileDesc, F_SETLKW, &lock) == -1)
  {
    perror("Unlock Error.\n");
    exit(EXIT_FAILURE);
  }

  if (close(fileDesc) < 0)
  {
    perror("CLOSE ERROR\n");
    exit(EXIT_FAILURE);
  }

  return 0;
}
