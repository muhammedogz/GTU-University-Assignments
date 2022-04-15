#ifndef COMMON_H
#define COMMON_H

/* Buffer Size for File Content - Both read and write */
#define BUFFER_SIZE 1024

/**
 * @brief Matrix struct
 */
typedef struct
{
  int id;
  int column;
  int row;
  int *data;
} Matrix;

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

  //
  INVALID_EXIT_STATUS,
} Error;

// Global error type to be used in the program
Error GLOBAL_ERROR;

/**
 * @brief Print error message
 *
 * @param error Error code
 */
void printError(Error error);

/**
 * @brief Print message with time stamp
 *
 * @param message Message to be printed
 */
void printMessageWithTime(char *message);

/**
 * @brief Detect if the given matrix is invertible or not
 *
 * @param matrix Matrix to be checked
 * @return int 1 if invertible, 0 otherwise
 */
int detectMatrixInvertible(const Matrix matrix);

#endif