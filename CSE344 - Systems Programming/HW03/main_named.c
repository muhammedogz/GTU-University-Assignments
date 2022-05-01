#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include "include/common.h"
#include "include/named.h"

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

  dprintf(STDOUT_FILENO, "fileContent: %s\n", fileContent);

  WholesalerBag wholesalerBag = convertToWholesalerBag(fileContent, fileSize);

  for (int i = 0; i < 5; i++)
  {
    dprintf(STDOUT_FILENO, "wholesalerBag.wholesaler[%d].ingredients[0]: %c\n", i, wholesalerBag.ingredients[i].ingredient1);
    dprintf(STDOUT_FILENO, "wholesalerBag.wholesaler[%d].ingredients[1]: %c\n", i, wholesalerBag.ingredients[i].ingredient2);
  }
}