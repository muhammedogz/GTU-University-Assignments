#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>
#include "include/common.h"
#include "include/serverY.h"

int main(int argc, char *argv[])
{
  char *pathToServerFifo = NULL;
  char *pathToLogFile = NULL;
  int poolSize = 0;
  int poolSize2 = 0;
  int time_v = 0;

  Matrix matrix;
  matrix.data = NULL;

  printMessageWithTime("ServerY started\n");

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToLogFile, &poolSize, &poolSize2, &time_v) == -1)
  {
    printError(GLOBAL_ERROR);
    invalid_usage();
    exit(EXIT_FAILURE);
  }

  matrix = readMatrix(pathToServerFifo);
  if (matrix.data == NULL)
  {
    printError(GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  printMessageWithTime("Received matrix\n");
}