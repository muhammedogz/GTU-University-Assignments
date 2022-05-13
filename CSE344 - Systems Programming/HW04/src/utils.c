#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdarg.h>
#include <stdlib.h>
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
static int semSetId;
static int globalC, globalN;
static char *globalInputFilePath;
static int globalRunningStatus = 1;

void sigint_handler(int signal)
{
  if (signal == SIGINT)
  {
    dprintf(STDOUT_FILENO, "%s: SIGINT received\n", getTime());
    globalRunningStatus = 0;
    freeResources();
    exit(0);
  }
}

int detectArguments(int argc, char *argv[])
{

  if (argc != 7)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int c_found = 0;
  int n_found = 0;
  int f_found = 0;
  while ((opt = getopt(argc, argv, "C:N:F:")) != -1)
  {
    switch (opt)
    {
    case 'C':
      if (c_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalC = atoi(optarg);
      c_found = 1;
      break;
    case 'N':
      if (n_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalN = atoi(optarg);
      n_found = 1;
      break;
    case 'F':
      if (f_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalInputFilePath = optarg;
      f_found = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!c_found || !n_found || !f_found)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  if (globalC <= 4 || globalN <= 1)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // print all
  // dprintf(STDOUT_FILENO, "C: %d\n", globalC);
  // dprintf(STDOUT_FILENO, "N: %d\n", globalN);
  // dprintf(STDOUT_FILENO, "F: %s\n", globalInputFilePath);

  return 1;
}

void *producerFunc()
{
  dprintf(STDOUT_FILENO, "%s: Producer is running\n", getTime());

  return NULL;
}

void *consumerFunc(void *arg)
{
  int id = *(int *)arg;
  dprintf(STDOUT_FILENO, "%s: Consumer-%d is running\n", getTime(), id);

  return NULL;
}

int initialize(char *inputFilePath, int C, int N)
{
  if ((semSetId = semget(IPC_PRIVATE, 2, S_IRUSR | S_IWUSR | IPC_CREAT | IPC_EXCL)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }

  unsigned short values[2] = {0, 0};
  semArgs.array = values;
  if (semctl(semSetId, 0, SETALL, semArgs) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }

  pthread_t consumers[C];
  pthread_t producer;

  globalC = C;
  globalN = N;
  globalInputFilePath = inputFilePath;

  // Producer thread creation
  if (pthread_create(&producer, NULL, producerFunc, NULL) != 0)
  {
    GLOBAL_ERROR = INVALID_THREAD_CREATION;
    return -1;
  }
  dprintf(STDOUT_FILENO, "%s: Suplier Created\n", getTime());

  // Detach Producer thread
  if (pthread_detach(producer) != 0)
  {
    GLOBAL_ERROR = INVALID_THREAD_DETACH;
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Suplier Detacthed\n", getTime());

  // Consumer threads creation
  for (int i = 0; i < C; i++)
  {
    if (pthread_create(&consumers[i], NULL, consumerFunc, (void *)&i) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_CREATION;
      return -1;
    }
    dprintf(STDOUT_FILENO, "%s: Consumer-%d: Created\n", getTime(), i);
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
  dprintf(STDOUT_FILENO, "%s: All Consumers Joined\n", getTime());
  // exit with pthread
  return 0;
}

int freeResources()
{
  // remove free semaphore
  if (semctl(semSetId, 0, IPC_RMID) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }

  return 0;
}

char *getTime()
{
  time_t rawtime;
  struct tm *timeinfo;
  time(&rawtime);
  timeinfo = localtime(&rawtime);
  char *timestamp = asctime(timeinfo);
  timestamp[strlen(timestamp) - 1] = '\0';

  return timestamp;
}

void printError(const int fd)
{
  printf("%s: ", getTime());
  printf("Error Code: %d\n", GLOBAL_ERROR);

  char *error_message = NULL;
  int show_perror = 1;
  switch (GLOBAL_ERROR)
  {
  case INVALID_MALLOC:
    error_message = "Invalid malloc";
    break;
  case INVALID_ARGUMENTS:
    error_message = "Invalid arguments";
    show_perror = 0;
    printUsage();
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
    dprintf(STDERR_FILENO, "Error Number: %d\n", GLOBAL_ERROR);
    break;
  }

  if (show_perror)
    perror(error_message);

  dprintf(fd, "%s\n", error_message);

  // terminate
  // exit(EXIT_FAILURE);
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: hw4 \n");
  dprintf(STDOUT_FILENO, "  -C NUMBER\t\tNumber of consumers. && C > 4\n");
  dprintf(STDOUT_FILENO, "  -N NUMBER\t\tNumber of loop time of consuemrs && N > 1\n");
  dprintf(STDOUT_FILENO, "  -F FILE\t\tInput file path\n");
}