#ifndef COMMON_H
#define COMMON_H

/* Buffer Size for File Content - Both read and write */
#define BUFFER_SIZE 1024

#define CHIEF_COUNT 6

#define SEMAPHORE_COUNT 11

typedef struct
{
  char ingredient1;
  char ingredient2;
} Ingredient;

typedef struct
{
  Ingredient *ingredients;
  int deliveredIngredient;
  int totalIngredients;
  int isMilk, isFlour, isWalnut, isSugar;
} WholesalerBag;

/**
 * @brief Error codes
 */
typedef enum
{
  INVALID_MALLOC = 1,
  FILE_OPEN_ERROR,
  FILE_READ_ERROR,
  FILE_WRITE_ERROR,
  FILE_LOCK_ERROR,
  FILE_UNLOCK_ERROR,
  FILE_CLOSE_ERROR,
  FILE_SEEK_ERROR,
  FILE_UNLINK_ERROR,
  INVALID_ARGUMENTS,
  INVALID_EXECVE,
  INVALID_FORK,
  INVALID_WAIT,
  INVALID_MATRIX,
  ALREADY_RUNNING,
  PIPE_CREATION_ERROR,
  FORK_ERROR,
  PIPE_READ_ERROR,
  PIPE_CLOSE_ERROR,
  PIPE_WRITE_ERROR,
  PRINT_ERROR,
  FIRST_INITIALIZE_SERVER,
  FILE_TRUNCATE_ERROR,
  FILE_MMAP_ERROR,
  UNLINK_ERROR,

  // unhandled
  SEMAPHORE_OPEN_ERROR,
  SEMAPHORE_UNLINK_ERROR,
  SEMAPHORE_CLOSE_ERROR,

  //
  INVALID_EXIT_STATUS,
} Error;

// Global error type to be used in the program
static Error GLOBAL_ERROR;

char *readFile(const char *fileName, int *fileSize);

WholesalerBag convertToWholesalerBag(char *fileContent, int fileSize);

void printError(const int fd, Error error);

char **generateNames(char *name);

#endif