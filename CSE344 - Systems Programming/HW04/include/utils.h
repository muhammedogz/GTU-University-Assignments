#ifndef UTILS_H
#define UTILS_H

typedef union SemArgUnion
{
  int val;
  struct semid_ds *buf;
  unsigned short *array;
} SemArgUnion;

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
  SEMAPHORE_OPEN_ERROR,
  SEMAPHORE_UNLINK_ERROR,
  SEMAPHORE_CLOSE_ERROR,
  WAITPID_ERROR,
  SEMAPHORE_INIT_FAILED,

  //
  INVALID_THREAD_CREATION,
  INVALID_THREAD_DETACH,
  INVALID_THREAD_JOIN,

  //
  INVALID_EXIT_STATUS,
} Error;

// Global error type to be used in the program
static Error GLOBAL_ERROR;

void printError(const int fd, Error error);

int printMessage(const int fd, const char *msg);

int printMessageWithTime(const int fd, char *message);

#endif // UTILS_H