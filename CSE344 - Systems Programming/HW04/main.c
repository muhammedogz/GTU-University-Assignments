#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include "include/utils.h"

int main(int argc, char *argv[])
{
  signal(SIGINT, sigint_handler);

  if (detectArguments(argc, argv) == -1)
  {
    printf("error: %s\n", "domates");
    printf("Global error in main: %d\n", GLOBAL_ERROR);
    printError(STDERR_FILENO);
    return -1;
  }

  if (initialize(NULL, 3, 3) == -1)
  {
    printError(STDERR_FILENO);
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Initialized\n", getTime());

  if (freeResources() == -1)
  {
    printError(STDERR_FILENO);
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Program finished\n", getTime());

  // wait for detached threads
  pthread_exit(NULL);
}