#ifndef COMMON_H
#define COMMON_H

#define BUFFER_SIZE 5000
#define PROC_STAT_PATH "/proc/self/stat"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <pthread.h>
#include <dirent.h>
#include <arpa/inet.h>
#include <sys/procfs.h>
#include <sys/syscall.h>
#include "list.h"

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
  INVALID_THREAD_CREATION,
  INVALID_THREAD_DETACH,
  INVALID_THREAD_JOIN,
  SEMAPHORE_GET_ERROR,
  SEMAPHORE_OPERATION_FAILED,
  MUTEX_INIT_ERROR,
  COND_INIT_ERROR,
  COND_BROADCAST_ERROR,
  COND_WAIT_ERROR,

  //
  SIGACTION_FAILURE,
  ATEXIT_FAILURE,

  //
  INVALID_EXIT_STATUS,
} Error;

/**
 * @brief Initialize a singal handler and assign a atexit function
 *
 * @param signalType Signal type
 * @param signalHandlerFunction Signal handler function
 * @param atexitFunction atexit function
 * @return int 0 on success, others on failure
 */
int initializeSignalAndAtexit(int signalType, void *signalHandlerFunction, void *atexitFunction);

/**
 * @brief Print error and exit
 *
 * @param fd file descriptor to print error
 */
void printError(const int fd, const Error error);

/**
 * @brief Get the File Size object
 *
 * @param filename
 * @return off_t
 */
off_t getFileSize(const char *filename);

/**
 * @brief Get the Time object
 *
 * @return char*
 */
char *getTime();

/**
 * @brief Get the Own Pid object
 *
 * @return pid_t
 */
pid_t getOwnPid();

#endif // COMMON_H