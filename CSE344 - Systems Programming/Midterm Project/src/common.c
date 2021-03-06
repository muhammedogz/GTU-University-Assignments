#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "../include/common.h"

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
  default:
    error_message = "Unknown error";
    char errorString[10];
    sprintf(errorString, "%d", error);
    printMessageWithTime(fd, "Unknown error: ");
    printMessage(fd, errorString);
    break;
  }

  if (show_perror)
    perror(error_message);

  printMessageWithTime(fd, error_message);
  printMessage(fd, "\n");

  // terminate
  // exit(EXIT_FAILURE);
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

void getCofactor(int **data, int **temp, int p, int q, int n)
{
  int i = 0, j = 0;
  for (int row = 0; row < n; row++)
  {
    for (int col = 0; col < n; col++)
    {
      if (row != p && col != q)
      {
        temp[i][j++] = data[row][col];
        if (j == n - 1)
        {
          j = 0;
          i++;
        }
      }
    }
  }
}

int findDeterminant(int **data, int n)
{
  int determinant = 0;
  int sign = 1;
  if (n == 1)
    return data[0][0];

  int **temp = (int **)malloc(sizeof(int *) * n);
  for (int i = 0; i < n; i++)
  {
    temp[i] = (int *)malloc(sizeof(int) * n);
  }

  for (int f = 0; f < n; f++)
  {
    getCofactor(data, temp, 0, f, n);
    determinant += sign * data[0][f] * findDeterminant(temp, n - 1);
    sign = -sign;
  }
  for (int i = 0; i < n; i++)
  {
    free(temp[i]);
  }
  free(temp);
  return determinant;
}

int detectMatrixInvertible(const Matrix matrix)
{
  // convert matrix to 2D array
  int **data = (int **)malloc(sizeof(int *) * matrix.row);
  for (int i = 0; i < matrix.row; i++)
  {
    data[i] = (int *)malloc(sizeof(int) * matrix.column);
  }

  for (int i = 0; i < matrix.row; i++)
  {
    for (int j = 0; j < matrix.column; j++)
    {
      data[i][j] = matrix.data[i * matrix.column + j];
    }
  }
  int determinant = findDeterminant(data, matrix.column);

  for (int i = 0; i < matrix.row; i++)
    free(data[i]);

  free(data);

  if (determinant == 0)
    return 0;
  else
    return 1;
}

int becomeDaemon(int flags)
{
  int maxfd, fd;
  switch (fork())
  {
  case -1:
    return -1;
  case 0:
    break;
  default:
    _exit(EXIT_SUCCESS);
  }
  if (setsid() == -1)
    return -1;
  switch (fork())
  {
  case -1:
    return -1;
  case 0:
    break;
  default:
    _exit(EXIT_SUCCESS);
  }
  if (!(flags & BD_NO_UMASK0))
    umask(0);
  if (!(flags & BD_NO_CHDIR))
    chdir("/");
  if (!(flags & BD_NO_CLOSE_FILES))
  {
    maxfd = sysconf(_SC_OPEN_MAX);
    if (maxfd == -1)
      maxfd = BD_MAX_CLOSE;
    for (fd = 0; fd < maxfd; fd++)
      close(fd);
  }
  if (!(flags & BD_NO_REOPEN_STD_FDS))
  {
    close(STDIN_FILENO);
    fd = open("/dev/null", O_RDWR);
    if (fd != STDIN_FILENO)
      return -1;
    if (dup2(STDIN_FILENO, STDOUT_FILENO) != STDOUT_FILENO)
      return -1;
    if (dup2(STDIN_FILENO, STDERR_FILENO) != STDERR_FILENO)
      return -1;
  }
  return 0;
}
