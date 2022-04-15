#include <stdio.h>
#include <stdlib.h>
#include "include/serverY.h"
#include "include/common.h"

int main(int argc, char *argv[])
{
  char *pathToServerFifo = NULL;
  char *pathToLogFile = NULL;
  int poolSize = 0;
  int poolSize2 = 0;
  int time_v = 0;

  printMessageWithTime("ServerY started\n");

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToLogFile, &poolSize, &poolSize2, &time_v) == -1)
  {
    printError(GLOBAL_ERROR);
    invalid_usage();
    exit(EXIT_FAILURE);
  }
}