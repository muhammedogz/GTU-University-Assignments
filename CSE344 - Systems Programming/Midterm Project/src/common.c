#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
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
  {
    free(data[i]);
  }
  free(data);

  if (determinant == 0)
    return 0;
  else
    return 1;
}
