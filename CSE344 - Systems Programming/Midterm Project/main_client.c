#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "include/common.h"
#include "include/client.h"

int main(int argc, char *argv[])
{
  char *pathToServerFifo = NULL;
  char *pathToDataFile = NULL;
  char *fileContent = NULL;
  int fileSize = 0;
  Matrix *matrix = NULL;

  int id = getpid();
  char idString[10];
  sprintf(idString, "%d", id);

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

  printMessageWithTime("Sended matrix\n");

  // if (detectInvertible(idString) == -1)
  // {
  //   printError(GLOBAL_ERROR);
  //   freeAndExit(fileContent, matrix, EXIT_FAILURE);
  // }

  freeAndExit(fileContent, matrix, EXIT_SUCCESS);

  return 0;
}