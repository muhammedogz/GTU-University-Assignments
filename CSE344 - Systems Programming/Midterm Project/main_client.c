#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "include/common.h"
#include "include/client.h"

int main(int argc, char *argv[])
{
  struct timespec start, end;
  char *pathToServerFifo = NULL;
  char *pathToDataFile = NULL;
  char *fileContent = NULL;
  int fileSize = 0;
  Matrix *matrix = NULL;

  char sizeStr[10];
  char idString[10];
  char passedSecond[10];

  int id = getpid();
  sprintf(idString, "%d", id);

  int printStatus = 0;
  clock_gettime(CLOCK_MONOTONIC, &start);
  // get exact time
  printStatus = printMessageWithTime(STDOUT_FILENO, "Client PID#");
  printStatus = printMessage(STDOUT_FILENO, idString);
  printStatus = printMessage(STDOUT_FILENO, " started\n");
  if (printStatus == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToDataFile) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    invalidUsage();
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  fileContent = readFile(pathToDataFile, &fileSize);
  if (fileContent == NULL)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
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
    printError(STDERR_FILENO, GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  sprintf(sizeStr, "%d", matrix->row);
  printStatus = printMessageWithTime(STDOUT_FILENO, "Client PID#");
  printStatus = printMessage(STDOUT_FILENO, idString);
  printStatus = printMessage(STDOUT_FILENO, " (\"");
  printStatus = printMessage(STDOUT_FILENO, pathToDataFile);
  printStatus = printMessage(STDOUT_FILENO, "\")");
  printStatus = printMessage(STDOUT_FILENO, " is submiting a ");
  printStatus = printMessage(STDOUT_FILENO, sizeStr);
  printStatus = printMessage(STDOUT_FILENO, "x");
  printStatus = printMessage(STDOUT_FILENO, sizeStr);
  printStatus = printMessage(STDOUT_FILENO, " matrix\n");
  if (printStatus == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  if (writeMatrix(pathToServerFifo, matrix) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  int invertible = detectInvertible(idString);
  if (invertible == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    freeAndExit(fileContent, matrix, EXIT_FAILURE);
  }

  // get collepsed time between start and end
  clock_gettime(CLOCK_MONOTONIC, &end);
  long long elapsed = (end.tv_sec - start.tv_sec) * 1000000000 + (end.tv_nsec - start.tv_nsec);
  sprintf(passedSecond, "%.2f", elapsed / (double)1000000000);

  const char *invertibleStr = invertible ? "invertible" : "not invertible";
  printStatus = printMessageWithTime(STDOUT_FILENO, "Client PID#");
  printStatus = printMessage(STDOUT_FILENO, idString);
  printStatus = printMessage(STDOUT_FILENO, " matrix is ");
  printStatus = printMessage(STDOUT_FILENO, invertibleStr);
  printStatus = printMessage(STDOUT_FILENO, " total time:");
  printStatus = printMessage(STDOUT_FILENO, passedSecond);
  printStatus = printMessage(STDOUT_FILENO, " seconds, goodbye\n");

  if (printStatus == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  freeAndExit(fileContent, matrix, EXIT_SUCCESS);

  return 0;
}