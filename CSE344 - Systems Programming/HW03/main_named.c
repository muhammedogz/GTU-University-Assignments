#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <signal.h>
#include "include/common.h"
#include "include/named.h"

// create pushers
// create chefs

int main(int argc, char *argv[])
{
  char *inputFilePath = NULL;
  char *name = NULL;
  char *fileContent = NULL;
  int fileSize = 0;

  if (detectArguments(argc, argv, &inputFilePath, &name) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  fileContent = readFile(inputFilePath, &fileSize);
  if (fileContent == NULL)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  char **names = generateNames(name);
  if (names == NULL)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  WholesalerBag wholesalerBag = convertToWholesalerBag(fileContent, fileSize);

  if (runNamed(wholesalerBag, names) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  free(fileContent);
  for (int i = 0; i < SEMAPHORE_COUNT; i++)
  {
    free(names[i]);
  }
  free(names);
  free(wholesalerBag.ingredients);
}