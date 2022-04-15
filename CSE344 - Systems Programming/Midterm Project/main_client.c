#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include "include/common.h"
#include "include/client.h"

int main(int argc, char *argv[])
{
  char *pathToServerFifo = NULL;
  char *pathToDataFile = NULL;
  char *fileContent = NULL;
  int fileSize = 0;
  char *clinetFifoConetent = NULL;
  int clientFifoSize = 0;
  Matrix *matrix = NULL;

  int id = getpid();
  char idString[10];
  sprintf(idString, "%d", id);

  int serverFifoDescriptor = 0;
  int clientFileDescriptor = 0;

  printMessageWithTime("Client started\n");

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToDataFile) == -1)
  {
    printError(GLOBAL_ERROR);
    invalidUsage();
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  fileContent = readFile(pathToDataFile, &fileSize);
  if (fileContent == NULL)
  {
    printError(GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  // remove all newlines from end
  while (fileContent[fileSize - 1] == '\n')
  {
    fileContent[fileSize - 1] = '\0';
    fileSize--;
  }

  matrix = convertToMatrix(fileContent, fileSize);
  if (matrix == NULL)
  {
    printError(GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  if (writeMatrix(pathToServerFifo, matrix) == -1)
  {
    printError(GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  if (detectInvertible(idString) == -1)
  {
    printError(GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  return 0;
}