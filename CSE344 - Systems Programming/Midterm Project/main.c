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

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToDataFile) == -1)
  {
    printError(GLOBAL_ERROR);
    invalidUsage();
    exit(EXIT_FAILURE);
  }

  write(STDOUT_FILENO, "Client started\n", strlen("Client started\n"));

  write(STDOUT_FILENO, "server fifo\n", strlen("server fifo\n"));
  write(STDOUT_FILENO, pathToServerFifo, strlen(pathToServerFifo));
  write(STDOUT_FILENO, "\n", 1);

  write(STDOUT_FILENO, "data file\n", strlen("data file\n"));
  write(STDOUT_FILENO, pathToDataFile, strlen(pathToDataFile));
  write(STDOUT_FILENO, "\n", 1);

  fileContent = readFile(pathToDataFile, &fileSize);
  if (fileContent == NULL)
  {
    printError(GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  write(STDOUT_FILENO, "file content\n", strlen("file content\n"));
  write(STDOUT_FILENO, fileContent, strlen(fileContent));
  write(STDOUT_FILENO, "\n", 1);

  return 0;
}