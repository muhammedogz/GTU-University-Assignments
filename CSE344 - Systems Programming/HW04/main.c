#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include "include/utils.h"

int main()
{

  if (initialize(NULL, 3, 3) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Initialized\n", getTime());

  if (freeResources() == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Program finished\n", getTime());
  pthread_exit(NULL); // wait for detached threads
}