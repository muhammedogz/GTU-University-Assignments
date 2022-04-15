#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "../include/common.h"

void printError(Error error)
{
  char *error_message = NULL;
  int show_perror = 1;
  switch (error)
  {
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
  case FILE_SEEK_ERROR:
    error_message = "File seek error";
    break;
  case INVALID_EXECVE:
    error_message = "Invalid execve";
    break;
  case INVALID_FORK:
    error_message = "Invalid fork";
    break;
  case INVALID_WAIT:
    error_message = "Invalid wait";
    show_perror = 0;
    break;
  case INVALID_MATRIX:
    error_message = "Invalid matrix. Matrix should be square";
    show_perror = 0;
    break;
  default:
    error_message = "Unknown error";
    break;
  }

  if (show_perror)
    perror(error_message);
  else
  {
    write(STDERR_FILENO, error_message, strlen(error_message));
    write(STDERR_FILENO, "\n", 1);
  }

  // terminate
  // exit(EXIT_FAILURE);
}

void printMessageWithTime(char *message)
{
  time_t rawtime;
  struct tm *timeinfo;
  time(&rawtime);
  timeinfo = localtime(&rawtime);
  char *timestamp = asctime(timeinfo);
  timestamp[strlen(timestamp) - 1] = '\0';

  write(STDOUT_FILENO, timestamp, strlen(timestamp));
  write(STDOUT_FILENO, ": ", 2);
  write(STDOUT_FILENO, message, strlen(message));
}
