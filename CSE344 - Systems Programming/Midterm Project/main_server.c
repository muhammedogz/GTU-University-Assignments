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

  int invertible;
  char clientFifo[10];

  Matrix matrix;
  matrix.data = NULL;

  // checks if server already running or not
  // if not, creates a temp file
  if (checkAlreadyRunning() == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  printMessageWithTime(STDOUT_FILENO, "ServerY started\n");

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToLogFile, &poolSize, &poolSize2, &time_v) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    invalid_usage();
    exitGracefully(EXIT_FAILURE, matrix);
  }

  matrix = readMatrix(pathToServerFifo);
  if (matrix.data == NULL)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    exitGracefully(EXIT_FAILURE, matrix);
  }

  printMessageWithTime(STDOUT_FILENO, "Received matrix\n");
  sprintf(clientFifo, "%d", matrix.id);
  invertible = detectMatrixInvertible(matrix);

  if (writeToClientFifo(clientFifo, invertible) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    exitGracefully(EXIT_FAILURE, matrix);
  }

  exitGracefully(EXIT_SUCCESS, matrix);
}