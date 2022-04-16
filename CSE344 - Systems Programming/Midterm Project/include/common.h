#ifndef COMMON_H
#define COMMON_H

/* Buffer Size for File Content - Both read and write */
#define BUFFER_SIZE 1024

#define WORKER_AVAILABLE 1
#define WORKER_BUSY 0

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
  ALREADY_RUNNING,
  PIPE_CREATION_ERROR,
  FORK_ERROR,
  PIPE_READ_ERROR,
  PIPE_CLOSE_ERROR,
  PIPE_WRITE_ERROR,
  PRINT_ERROR,

  FILE_TRUNCATE_ERROR,
  FILE_MMAP_ERROR,

  //
  INVALID_EXIT_STATUS,
} Error;

// Global error type to be used in the program
Error GLOBAL_ERROR;

/**
 * @brief Print error message
 *
 * @param fd File descriptor
 * @param error Error code
 */
void printError(const int fd, Error error);

/**
 * @brief Print message with time stamp
 *
 * @param fd File descriptor
 * @param message Message to be printed
 * @return int 0 on success, -1 on failure
 */
int printMessageWithTime(const int fd, char *message);

/**
 * @brief Print message to stdout with write
 *
 * @param fd File descriptor
 * @param msg Message to be printed
 * @return int 0 on success, -1 on failure
 */
int printMessage(const int fd, const char *msg);

/**
 * @brief Detect if the given matrix is invertible or not
 *
 * @param matrix Matrix to be checked
 * @return int 1 if invertible, 0 otherwise
 */
int detectMatrixInvertible(const Matrix matrix);

#endif