#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include "include/common.h"
#include "include/client.h"

int main(int argc, char *argv[])
{
  char *pathToServerFifo = NULL;
  char *pathToDataFile = NULL;
  char *fileContent = NULL;
  int fileSize = 0;
  Matrix *matrix = NULL;

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToDataFile) == -1)
  {
    printError(GLOBAL_ERROR);
    invalidUsage();
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  write(STDOUT_FILENO, "Client started\n", strlen("Client started\n"));

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

  if (mkfifo(pathToServerFifo, 0666) == -1)
  {
    if (errno != EEXIST)
    {
      GLOBAL_ERROR = FILE_OPEN_ERROR;
      printError(GLOBAL_ERROR);
      freeAndExit(fileContent, matrix, EXIT_FAILURE);
    }
  }

  int serverFifoDescriptor = open(pathToServerFifo, O_WRONLY);
  if (serverFifoDescriptor == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    printError(GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  // write matrix to server
  write(serverFifoDescriptor, matrix, sizeof(Matrix));

  // close
  close(serverFifoDescriptor);

  // read same matrix from server
  serverFifoDescriptor = open(pathToServerFifo, O_RDONLY);
  if (serverFifoDescriptor == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    printError(GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  Matrix *result = malloc(sizeof(Matrix));
  read(serverFifoDescriptor, result, sizeof(Matrix));

  // do same thing with printf
  printf("Matrix:\n");
  for (int i = 0; i < result->row; i++)
  {
    for (int j = 0; j < result->column; j++)
    {
      printf("%d ", result->data[i * result->column + j]);
    }
    printf("\n");
  }

  freeAndExit(fileContent, matrix, EXIT_SUCCESS);

  return 0;
}