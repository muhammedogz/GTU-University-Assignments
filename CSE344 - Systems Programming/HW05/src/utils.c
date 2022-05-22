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
#include "../include/utils.h"

static char *globalInputFilePath1, *globalInputFilePath2;
static char *globalOutputFilePath;
static int globalN, globalM;
static int globalRunningStatus = 1;

void sigint_handler(int signal)
{
  if (signal == SIGINT)
  {
    globalRunningStatus = 0;
    freeResources();
    dprintf(STDOUT_FILENO, "%s: SIGINT received. All resources are cleaned\n", getTime());
    exit(0);
  }
}

int detectArguments(int argc, char *argv[])
{

  if (argc != 11)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int i_found = 0;
  int j_found = 0;
  int o_found = 0;
  int n_found = 0;
  int m_found = 0;
  while ((opt = getopt(argc, argv, "i:j:o:n:m:")) != -1)
  {
    switch (opt)
    {
    case 'i':
      if (i_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalInputFilePath1 = optarg;
      i_found = 1;
      break;
    case 'j':
      if (j_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalInputFilePath2 = optarg;
      j_found = 1;
      break;
    case 'o':
      if (o_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalOutputFilePath = optarg;
      o_found = 1;
      break;
    case 'n':
      if (n_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalN = atoi(optarg);
      n_found = 1;
      break;
    case 'm':
      if (m_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalM = atoi(optarg);
      m_found = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!i_found || !j_found || !o_found || !n_found || !m_found)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // if (globalC <= 4 || globalN <= 1)
  // {
  //   GLOBAL_ERROR = INVALID_ARGUMENTS;
  //   return -1;
  // }

  // Is it necessary to check file size equal to globalTotalSize?
  // int fileSize = getFileSize(globalInputFilePath);
  // if (fileSize != globalTotalSize)
  // {
  //   GLOBAL_ERROR = INVALID_ARGUMENTS;
  //   return -1;
  // }

  // print all
  // dprintf(STDOUT_FILENO, "%s: Input file 1: %s\n", getTime(), globalInputFilePath1);
  // dprintf(STDOUT_FILENO, "%s: Input file 2: %s\n", getTime(), globalInputFilePath2);
  // dprintf(STDOUT_FILENO, "%s: Output file: %s\n", getTime(), globalOutputFilePath);
  // dprintf(STDOUT_FILENO, "%s: N: %d\n", getTime(), globalN);
  // dprintf(STDOUT_FILENO, "%s: M: %d\n", getTime(), globalM);

  return 1;
}

int freeResources()
{

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
  case INVALID_THREAD_CREATION:
    error_message = "Invalid thread creation";
    break;
  case INVALID_THREAD_JOIN:
    error_message = "Invalid thread join";
    break;
  case INVALID_THREAD_DETACH:
    error_message = "Invalid thread detach";
    break;
  case SEMAPHORE_GET_ERROR:
    error_message = "Semaphore get error";
    break;
  case SEMAPHORE_OPERATION_FAILED:
    error_message = "Semaphore operation failed";
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
  dprintf(STDOUT_FILENO, "Usage: hw5 \n");
  dprintf(STDOUT_FILENO, "  -i filePath1\t\t Filepath of the first file\n");
  dprintf(STDOUT_FILENO, "  -j filePath2\t\t Filepath of the second file\n");
  dprintf(STDOUT_FILENO, "  -o output\t\t Filepath of the output file\n");
  dprintf(STDOUT_FILENO, "  -n NUMBER\t\t Number of times to read input\n");
  dprintf(STDOUT_FILENO, "  -m NUMBER\t\t Number of threads\n");
}

off_t getFileSize(const char *filename)
{
  struct stat st;

  if (stat(filename, &st) == 0)
    return st.st_size;

  return -1;
}