#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include "include/common.h"
#include "include/unnamed.h"

int main(int argc, char *argv[])
{
  char *inputFilePath = NULL;
  char *fileContent = NULL;
  int fileSize = 0;

  if (detectArguments(argc, argv, &inputFilePath) == -1)
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

  WholesalerBag wholesalerBag = convertToWholesalerBag(fileContent, fileSize);

  if (runUnNamed(wholesalerBag) == -1)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  free(fileContent);
  free(wholesalerBag.ingredients);
  return 0;
}
