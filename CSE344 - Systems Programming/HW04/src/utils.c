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
static int globalTotalSize = 0;
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

  // N*C times 1, N*C times 2
  globalTotalSize = globalC * globalN * 2;

  // Is it necessary to check file size equal to globalTotalSize?
  // int fileSize = getFileSize(globalInputFilePath);
  // if (fileSize != globalTotalSize)
  // {
  //   GLOBAL_ERROR = INVALID_ARGUMENTS;
  //   return -1;
  // }

  // print all
  // dprintf(STDOUT_FILENO, "C: %d\n", globalC);
  // dprintf(STDOUT_FILENO, "N: %d\n", globalN);
  // dprintf(STDOUT_FILENO, "F: %s\n", globalInputFilePath);

  return 1;
}

int initialize()
{
  // Initialize Semaphores
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

  // Initialize Threads
  pthread_t consumers[globalC];
  pthread_t producer;

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
  for (int i = 0; i < globalC; i++)
  {
    if (pthread_create(&consumers[i], NULL, consumerFunc, (void *)&i) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_CREATION;
      return -1;
    }
    dprintf(STDOUT_FILENO, "%s: Consumer-%d: Created\n", getTime(), i);
  }

  // Join Consumer threads
  for (int i = 0; i < globalC; i++)
  {
    if (pthread_join(consumers[i], NULL) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_JOIN;
      return -1;
    }
  }
  dprintf(STDOUT_FILENO, "%s: All Consumers Joined\n", getTime());
  return 0;
}

void *producerFunc()
{
  dprintf(STDOUT_FILENO, "%s: Producer is running\n", getTime());

  int fd = 0;
  int product1 = 0, product2 = 0;
  char produced = '\0';

  // INCREMENTING THE VALUE OF SEMAPHORE WITH SEMCTL
  struct sembuf postOperation;
  postOperation.sem_num = 0;
  postOperation.sem_op = 1;
  postOperation.sem_flg = 0;

  if ((fd = open(globalInputFilePath, O_RDONLY, S_IRUSR | S_IRGRP | S_IRGRP)) == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  for (int i = 0; i < globalTotalSize; ++i)
  {

    int readValue = read(fd, &produced, 1);

    if (readValue == -1)
    {
      GLOBAL_ERROR = FILE_READ_ERROR;
      return NULL;
    }
    if (readValue == 0)
      break;

    if ((product1 = getSemValue(0)) == -1)
      break;

    if ((product2 = getSemValue(1)) == -1)
      break;

    dprintf(STDOUT_FILENO, "%s: Supplier: read from input a '%c'. Current amounts: %d x '1', %d x '2'.\n", getTime(), produced, product1, product2);

    // Post Operations
    if (produced == '1')
    {
      postOperation.sem_num = 0;
      if (semop(semSetId, &postOperation, 1) == -1)
      {
        GLOBAL_ERROR = SEMAPHORE_OPERATION_FAILED;
        printError(STDERR_FILENO);
        return NULL;
      }
    }
    else if (produced == '2')
    {
      postOperation.sem_num = 1;
      if (semop(semSetId, &postOperation, 1) == -1)
      {
        GLOBAL_ERROR = SEMAPHORE_OPERATION_FAILED;
        printError(STDERR_FILENO);
        return NULL;
      }
    }

    if ((product1 = getSemValue(0)) == -1)
      break;

    if ((product2 = getSemValue(1)) == -1)
      break;

    dprintf(STDOUT_FILENO, "%s: Supplier: delivered a '%c'. Post-delivery amounts: %d x '1', %d x '2'.\n", getTime(), produced, product1, product2);
  }

  if (close(fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    printError(STDERR_FILENO);
    return NULL;
  }

  if (product1 == -1 || product2 == -1)
    printError(STDERR_FILENO);

  dprintf(STDOUT_FILENO, "%s: Supplier: finished.\n", getTime());

  return NULL;
}

void *consumerFunc(void *arg)
{
  int product1 = 0, product2 = 0;
  int consumerId = *(int *)arg;

  struct sembuf waitOperation[2];
  waitOperation[0].sem_num = 0;
  waitOperation[0].sem_op = -1;
  waitOperation[0].sem_flg = 0;

  waitOperation[1].sem_num = 1;
  waitOperation[1].sem_op = -1;
  waitOperation[1].sem_flg = 0;

  dprintf(STDOUT_FILENO, "%s: Consumer-%d is running\n", getTime(), consumerId);

  for (int i = 0; i < globalN; i++)
  {

    if ((product1 = getSemValue(0)) == -1)
      break;
    if ((product2 = getSemValue(1)) == -1)
      break;

    dprintf(STDOUT_FILENO, "%s: Consumer-%d at iteration %d (waiting). Current amounts: %d x '1', %d x '2'.\n", getTime(), consumerId, i, product1, product2);

    if (semop(semSetId, waitOperation, 2) == -1)
    {
      GLOBAL_ERROR = SEMAPHORE_OPERATION_FAILED;
      printError(STDERR_FILENO);
      return NULL;
    }

    if ((product1 = getSemValue(0)) == -1)
      break;

    if ((product2 = getSemValue(1)) == -1)
      break;

    dprintf(STDOUT_FILENO, "%s: Consumer-%d at iteration %d (consumed). Post-consumption amounts: %d x '1', %d x '2'.\n", getTime(), consumerId, i, product1, product2);
  }
  dprintf(STDOUT_FILENO, "%s: Consumer-%d has left\n", getTime(), consumerId);

  return NULL;
}

int getSemValue(int semNum)
{
  int semValue;

  if ((semValue = semctl(semSetId, semNum, GETVAL)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_GET_ERROR;
    return -1;
  }

  return semValue;
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
  dprintf(STDOUT_FILENO, "Also notice that, your file size should equal to your N*C*2 byte.\n");
}

off_t getFileSize(const char *filename)
{
  struct stat st;

  if (stat(filename, &st) == 0)
    return st.st_size;

  return -1;
}