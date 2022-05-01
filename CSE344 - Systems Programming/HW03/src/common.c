#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "../include/common.h"

char *readFile(const char *fileName, int *fileSize)
{

  int fileDescriptor = 0;
  char *fileContent = NULL;
  int fileSizeTemp = 0;
  int readSize = 0;
  int readCount = 0;
  int readTotal = 0;
  char readBuffer[BUFFER_SIZE];

  if ((fileDescriptor = open(fileName, O_RDONLY, fileSize)) < 0)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  fileSizeTemp = lseek(fileDescriptor, 0, SEEK_END);
  if (fileSizeTemp < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }

  fileContent = (char *)malloc(fileSizeTemp + 1);
  if (fileContent == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  if (lseek(fileDescriptor, 0, SEEK_SET) < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }
  while ((readSize = read(fileDescriptor, readBuffer, BUFFER_SIZE)))
  {
    if (readSize < 0)
    {
      GLOBAL_ERROR = FILE_READ_ERROR;
      return NULL;
    }
    readCount = 0;
    while (readCount < readSize)
    {
      fileContent[readTotal] = readBuffer[readCount];
      readCount++;
      readTotal++;
    }
  }

  fileContent[readTotal] = '\0';

  *fileSize = readTotal;
  // close
  int closeRes = close(fileDescriptor);
  if (closeRes < 0)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return fileContent;
}

WholesalerBag convertToWholesalerBag(char *fileContent, int fileSize)
{
  // remove all newlines from end
  while (fileContent[fileSize - 1] == '\n')
  {
    fileContent[fileSize - 1] = '\0';
    fileSize--;
  }

  // find line count in fileContent
  int ingredientCount = 0;
  for (int i = 0; i < fileSize; i++)
  {
    if (fileContent[i] == '\n')
    {
      ingredientCount++;
    }
  }
  ingredientCount++;

  WholesalerBag wholesalerBag;
  wholesalerBag.totalIngredients = ingredientCount;
  wholesalerBag.deliveredIngredient = 0;
  wholesalerBag.isFlour = 0, wholesalerBag.isSugar = 0, wholesalerBag.isMilk = 0, wholesalerBag.isWalnut = 0;
  wholesalerBag.ingredients = (Ingredient *)malloc(sizeof(Ingredient) * ingredientCount);
  if (wholesalerBag.ingredients == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return wholesalerBag;
  }

  // assign values to ingredients
  int ingredientIndex = 0;
  for (int i = 0; i < ingredientCount; i++)
  {
    wholesalerBag.ingredients[i].ingredient1 = fileContent[ingredientIndex++];
    wholesalerBag.ingredients[i].ingredient2 = fileContent[ingredientIndex++];
    ingredientIndex++;
  }

  return wholesalerBag;
}

void printError(const int fd, Error error)
{
  char *error_message = NULL;
  int show_perror = 1;
  switch (error)
  {
  case INVALID_MALLOC:
    error_message = "Invalid malloc";
    break;
  case INVALID_ARGUMENTS:
    error_message = "Invalid arguments";
    show_perror = 0;
    break;
  case FILE_OPEN_ERROR:
    error_message = "File open error";
    break;
  case FILE_WRITE_ERROR:
    error_message = "File write error";
    break;
  case FILE_READ_ERROR:
    error_message = "File read error";
    break;
  case FILE_LOCK_ERROR:
    error_message = "File lock error";
    break;
  case FILE_UNLOCK_ERROR:
    error_message = "File unlock error";
    break;
  case FILE_CLOSE_ERROR:
    error_message = "File close error";
    break;
  case FILE_UNLINK_ERROR:
    error_message = "File unlink error";
    break;
  case ALREADY_RUNNING:
    error_message = "Server already running, If it is not running, delete serverYTemp file.";
    break;
  case FILE_SEEK_ERROR:
    error_message = "File seek error";
    break;
  case INVALID_EXECVE:
    error_message = "Invalid execve";
    break;
  case INVALID_FORK:
    error_message = "Invalid fork";
    break;
  case PIPE_CREATION_ERROR:
    error_message = "Pipe creation error";
    break;
  case FORK_ERROR:
    error_message = "Fork error";
    break;
  case PIPE_READ_ERROR:
    error_message = "Pipe read error";
    break;
  case PIPE_CLOSE_ERROR:
    error_message = "Pipe close error";
    break;
  case PIPE_WRITE_ERROR:
    error_message = "Pipe write error";
    break;
  case INVALID_WAIT:
    error_message = "Invalid wait";
    show_perror = 0;
    break;
  case INVALID_MATRIX:
    error_message = "Invalid matrix. Matrix should be square (NxN). N >= 2";
    show_perror = 0;
    break;
  case PRINT_ERROR:
    error_message = "Print error";
    break;
  case FIRST_INITIALIZE_SERVER:
    error_message = "First start serverY. serverY is not working now.";
    show_perror = 0;
    break;
  case FILE_TRUNCATE_ERROR:
    error_message = "File truncate error";
    break;
  case FILE_MMAP_ERROR:
    error_message = "File mmap error";
    break;
  case UNLINK_ERROR:
    error_message = "Unlink error";
    break;
  default:
    error_message = "Unknown error";
    char errorString[10];
    sprintf(errorString, "%d", error);
    break;
  }

  if (show_perror)
    perror(error_message);

  dprintf(fd, "%s\n", error_message);

  // terminate
  // exit(EXIT_FAILURE);
}

char **generateNames(char *name)
{
  char **names = (char **)malloc(sizeof(char *) * SEMAPHORE_COUNT);
  if (names == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  // add i to name
  for (int i = 0; i < SEMAPHORE_COUNT; i++)
  {
    names[i] = (char *)malloc(sizeof(char) * (strlen(name) + 2));
    if (names[i] == NULL)
    {
      GLOBAL_ERROR = INVALID_MALLOC;
      return NULL;
    }
    strcpy(names[i], name);
    char iString[2];
    sprintf(iString, "%d", i);
    strcat(names[i], iString);
    dprintf(STDOUT_FILENO, "name: %s\n", names[i]);
  }

  return names;
}
