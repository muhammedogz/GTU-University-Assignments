#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "include/common.h"
#include "include/serverY.h"

int main(int argc, char *argv[])
{

  char *pathToServerFifo = NULL;
  char *pathToLogFile = NULL;
  int poolSize = 0;
  int poolSize2 = 0;
  int time_v = 0;

  int logFileDescriptor = 0;

  int invertible;
  char clientFifo[10];

  Matrix matrix;
  matrix.data = NULL;

  // checks if server already running or not
  // if not, creates a temp file
  if (checkAlreadyRunning() == -1)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToLogFile, &poolSize, &poolSize2, &time_v) == -1)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    invalid_usage();
    exitGracefully(EXIT_FAILURE, matrix);
  }

  // open log file
  logFileDescriptor = open(pathToLogFile, O_WRONLY | O_APPEND | O_CREAT, 0666);
  if (logFileDescriptor == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    printError(logFileDescriptor, GLOBAL_ERROR);
    exitGracefully(EXIT_FAILURE, matrix);
  }

  printMessageWithTime(logFileDescriptor, "ServerY started\n");

  matrix = readMatrix(pathToServerFifo);
  if (matrix.data == NULL)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    exitGracefully(EXIT_FAILURE, matrix);
  }

  printMessageWithTime(logFileDescriptor, "Received matrix\n");
  sprintf(clientFifo, "%d", matrix.id);
  invertible = detectMatrixInvertible(matrix);

  if (writeToClientFifo(clientFifo, invertible) == -1)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    exitGracefully(EXIT_FAILURE, matrix);
  }

  exitGracefully(EXIT_SUCCESS, matrix);
}