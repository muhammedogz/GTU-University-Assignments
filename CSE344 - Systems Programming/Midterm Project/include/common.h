#ifndef COMMON_H
#define COMMON_H

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
  INVALID_ARGUMENTS,
  INVALID_EXECVE,
  INVALID_FORK,
  INVALID_WAIT,

  //
  INVALID_EXIT_STATUS,
} Error;

// Global error type to be used in the program
Error GLOBAL_ERROR;

void printError(Error error);

#endif