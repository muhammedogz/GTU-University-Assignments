#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <pthread.h>
#include <fcntl.h>
#include <errno.h> // spesific error message
#include "../include/utils.h"

SemArgUnion semArgs;
static int semId;
static int C, N;
static char *filePath;
static volatile sig_atomic_t didSigIntCome = 0;

int initialize(int cNumber, int nNumber, char *path)
{
  if ((semId = semget(IPC_PRIVATE, 2, S_IRUSR | S_IWUSR | IPC_CREAT | IPC_EXCL)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }

  unsigned short values[2] = {0, 0};
  semArgs.array = 0;
  if (semctl(semId, 0, SETALL, semArgs) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }

  pthread_attr_t threadAttributes;
  pthread_t consumers[cNumber];
  pthread_t producer;

  pthread_attr_init(&threadAttributes);
  pthread_attr_setscope(&threadAttributes, PTHREAD_SCOPE_SYSTEM);

  N = nNumber;
  C = cNumber;
  filePath = path;

  // Consumer threads creation
  for (int i = 0; i < C; i++)
  {
    if (pthread_create(&consumers[i], NULL, consumerFunc, NULL) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_CREATION;
      return -1;
    }
  }
  printMessageWithTime(STDOUT_FILENO, "Creating supplier\n");

  // Producer thread creation
  if (pthread_create(&producer, NULL, producerFunc, NULL) != 0)
  {
    GLOBAL_ERROR = INVALID_THREAD_CREATION;
    return -1;
  }

  // Detach Producer thread
  if (pthread_detach(producer) != 0)
  {
    GLOBAL_ERROR = INVALID_THREAD_DETACH;
    return -1;
  }

  // Join Consumer threads
  for (int i = 0; i < C; i++)
  {
    if (pthread_join(consumers[i], NULL) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_JOIN;
      return -1;
    }
  }

  // exit with pthread
  pthread_exit(NULL);
  return 0;
}

int printMessageWithTime(const int fd, char *message)
{
  time_t rawtime;
  struct tm *timeinfo;
  time(&rawtime);
  timeinfo = localtime(&rawtime);
  char *timestamp = asctime(timeinfo);
  timestamp[strlen(timestamp) - 1] = '\0';

  if (write(fd, timestamp, strlen(timestamp)) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }

  if (write(fd, ": ", 2) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }
  if (write(fd, message, strlen(message)) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }
  return 0;
}

int printMessage(const int fd, const char *msg)
{
  if (write(fd, msg, strlen(msg)) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }
  return 0;
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
  case SEMAPHORE_OPEN_ERROR:
    error_message = "Semaphore open error";
    break;
  case SEMAPHORE_CLOSE_ERROR:
    error_message = "Semaphore close error";
    break;
  case SEMAPHORE_UNLINK_ERROR:
    error_message = "Semaphore unlink error";
    break;
  case WAITPID_ERROR:
    error_message = "Waitpid error";
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