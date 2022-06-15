#include "../include/common.h"

int initializeSignalAndAtexit(int signalType, void *signalHandlerFunction, void *atexitFunction)
{
  struct sigaction sigAction;
  memset(&sigAction, 0, sizeof(sigAction));
  sigAction.sa_handler = signalHandlerFunction;
  if (sigaction(signalType, &sigAction, NULL) < 0)
  {
    return SIGACTION_FAILURE;
  }
  if (atexit(atexitFunction) != 0)
  {
    return ATEXIT_FAILURE;
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

void printError(const int fd, const Error error)
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
  case MUTEX_INIT_ERROR:
    error_message = "Mutex init error";
    break;
  case COND_INIT_ERROR:
    error_message = "Cond init error";
    break;
  case COND_BROADCAST_ERROR:
    error_message = "Cond broadcast error";
    break;
  case COND_WAIT_ERROR:
    error_message = "Cond wait error";
    break;
  default:
    error_message = "Unknown error";
    dprintf(STDERR_FILENO, "Error Number: %d\n", error);
    break;
  }

  if (show_perror)
    perror(error_message);

  dprintf(fd, "%s\n", error_message);

  // terminate
  // exit(EXIT_FAILURE);
}

off_t getFileSize(const char *filename)
{
  struct stat st;

  if (stat(filename, &st) == 0)
    return st.st_size;

  return -1;
}