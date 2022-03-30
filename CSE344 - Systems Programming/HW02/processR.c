#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "processR.h"

static volatile sig_atomic_t sig_flag = 0;

// only hanlde SIGINT func
static void sig_handler(int signo)
{
  if (signo == SIGINT)
    sig_flag = 1;
}

extern char **environ;

int main(int argc, char *argv[])
{

  struct sigaction sig_int_action;
  memset(&sig_int_action, 0, sizeof(sig_int_action));
  sig_int_action.sa_handler = sig_handler;
  sig_int_action.sa_flags = 0;
  sigaction(SIGINT, &sig_int_action, NULL);

  char *output_file = argv[2];
  int file_descriptor;
  struct flock lock;

  if ((file_descriptor = open(output_file, O_WRONLY | O_APPEND, S_IWGRP)) < 0)
  {
    perror("OPEN ERROR\n");
    exit(EXIT_FAILURE);
  }

  memset(&lock, 0, sizeof(lock));
  lock.l_type = F_WRLCK;
  if (fcntl(file_descriptor, F_SETLKW, &lock) == -1)
  {
    perror("Lock Error.\n");
    exit(EXIT_FAILURE);
  }

  Matrix *matrix = convert_to_matrix();
  calculate_and_write_to_file(file_descriptor, matrix, 10);

  lock.l_type = F_UNLCK;
  if (fcntl(file_descriptor, F_SETLKW, &lock) == -1)
  {
    perror("Unlock Error.\n");
    exit(EXIT_FAILURE);
  }

  if (close(file_descriptor) < 0)
  {
    perror("CLOSE ERROR\n");
    exit(EXIT_FAILURE);
  }

  // print environ

  // free matrix
  free(matrix);

  return 0;
}

Matrix *convert_to_matrix()
{
  // convert environ to matrix
  Matrix *matrix = malloc(sizeof(Matrix) * 10);

  for (int i = 0; environ[i] != NULL; i++)
  {
    char *token = strtok(environ[i], ",");

    matrix[i].x = atoi(token);
    token = strtok(NULL, ",");
    matrix[i].y = atoi(token);
    token = strtok(NULL, ",");
    matrix[i].z = atoi(token);
    token = strtok(NULL, ",");
  }

  // print matrix
  // for (int i = 0; i < 3; i++)
  // {
  //   printf("matrix[%d] = %d %d %d\n", i, matrix[i].x, matrix[i].y, matrix[i].z);
  //   printf("matrix str: %s %s %s\n", int_to_string(matrix[i].x), int_to_string(matrix[i].y), int_to_string(matrix[i].z));
  // }

  return matrix;
}

void calculate_and_write_to_file(int file_descriptor, Matrix *matrix, int matrix_count)
{
  double covariance_matrix[3][3];

  for (int i = 0; i < 3; i++)
  {
    for (int j = 0; j < 3; j++)
    {
      covariance_matrix[i][j] = 0;
    }
  }

  for (int i = 0; i < 3; i++)
  {
    for (int j = 0; j < 3; j++)
    {
      double average_1 = 0;
      double average_2 = 0;
      int covariance = 0;

      for (int k = 0; k < matrix_count; k++)
      {
        average_1 += determine_which_letter(i, matrix[k]);
        average_2 += determine_which_letter(j, matrix[k]);
      }
      average_1 /= matrix_count;
      average_2 /= matrix_count;

      for (int k = 0; k < matrix_count; k++)
      {
        covariance += (determine_which_letter(i, matrix[k]) - average_1) * (determine_which_letter(j, matrix[k]) - average_2);
      }
      covariance_matrix[i][j] = covariance / matrix_count;
    }
  }

  // print to file
  char *covariance_matrix_str = malloc(sizeof(char) * 100);
  for (int i = 0; i < 3; i++)
  {
    for (int j = 0; j < 3; j++)
    {
      const *buffer = gcvt(covariance_matrix[i][j], 10, buffer);
      strcat(covariance_matrix_str, buffer);
      strcat(covariance_matrix_str, " ");
    }
    strcat(covariance_matrix_str, "\n");
  }

  if (write(file_descriptor, covariance_matrix_str, strlen(covariance_matrix_str)) < 0)
  {
    perror("WRITE ERROR\n");
    exit(EXIT_FAILURE);
  }

  free(covariance_matrix_str);

  return;
}

char *int_to_string(int i)
{
  if (i == 0)
  {
    return "0";
  }

  char *str = (char *)malloc(10);
  if (str == NULL)
  {
    perror("MALLOC ERROR\n");
    return NULL;
  }

  int j = 0;
  while (i > 0)
  {
    str[j++] = i % 10 + '0';
    i /= 10;
  }
  str[j] = '\0';

  char *str_reversed = (char *)malloc(j + 1);
  if (str_reversed == NULL)
  {
    perror("MALLOC ERROR\n");
    return NULL;
  }

  int k = 0;
  for (int l = j - 1; l >= 0; l--)
  {
    str_reversed[k++] = str[l];
  }
  str_reversed[k] = '\0';

  free(str);

  return str_reversed;
}

int determine_which_letter(int i, Matrix matrix)
{
  if (i == 0)
  {
    return matrix.x;
  }
  else if (i == 1)
  {
    return matrix.y;
  }
  else if (i == 2)
  {
    return matrix.z;
  }
}
