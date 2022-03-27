#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "frobenius.h"

int detect_arguments(int argc, char *argv[], char **inputFilePath, char **outputFilePath)
{
  if (argc != 5 || strcmp(argv[1], "-i") != 0 || strcmp(argv[3], "-o") != 0)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  *inputFilePath = argv[2];
  *outputFilePath = argv[4];

  return 1;
}

char *read_file(char *file_name, int *file_size)
{

  int file_descriptor = 0;
  char *file_content = NULL;
  int file_size_int = 0;
  int read_size = 0;
  int read_count = 0;
  int read_total = 0;
  char read_buffer[BUFFER_SIZE];

  if ((file_descriptor = open(file_name, O_RDONLY, 0)) < 0)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  file_size_int = lseek(file_descriptor, 0, SEEK_END);
  if (file_size_int < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }

  file_content = (char *)malloc(file_size_int + 1);
  if (file_content == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  if (lseek(file_descriptor, 0, SEEK_SET) < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }
  while (read_total < file_size_int)
  {
    read_size = read(file_descriptor, read_buffer, BUFFER_SIZE);
    if (read_size < 0)
    {
      GLOBAL_ERROR = FILE_READ_ERROR;
      return NULL;
    }

    read_count = 0;
    while (read_count < read_size)
    {
      file_content[read_total++] = read_buffer[read_count++];
    }
  }

  file_content[read_total] = '\0';

  *file_size = read_total;
  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return file_content;
}

int write_file(char *file_name, char *file_content, int file_size)
{
  int write_size = 0;
  int write_count = 0;
  int write_total = 0;
  char write_buffer[BUFFER_SIZE];

  int file_descriptor = open(file_name, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
  if (file_descriptor < 0)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  if (lseek(file_descriptor, 0, SEEK_SET) < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return -1;
  }

  while (write_total < file_size)
  {
    write_count = 0;
    while (write_count < BUFFER_SIZE && write_total < file_size)
    {
      write_buffer[write_count++] = file_content[write_total++];
    }

    write_size = write(file_descriptor, write_buffer, write_count);
    if (write_size < 0)
    {
      GLOBAL_ERROR = FILE_WRITE_ERROR;
      return -1;
    }
  }

  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return -1;
  }

  return 0;
}

int lock_file(int file_desc)
{
  struct flock lock;
  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;

  if (fcntl(file_desc, F_SETLK, &lock) < 0)
  {
    GLOBAL_ERROR = FILE_LOCK_ERROR;
    return -1;
  }

  return 1;
  // int lock_res = lockf(file_descriptor, F_TLOCK, 0);
  // if (lock_res < 0)
  // {
  //   perror("lockf");
  //   exit(EXIT_FAILURE);
  // }
}

void print_error_and_exit(const Error error)
{
  char *error_message = NULL;
  int show_perror = 1;
  switch (error)
  {
  case INVALID_ARGUMENTS:
    error_message = "Invalid arguments";
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
  case FILE_SEEK_ERROR:
    error_message = "File seek error";
    break;
  default:
    error_message = "Unknown error";
    break;
  }

  if (show_perror)
  {
    perror(error_message);
  }
  else
  {
    write(STDERR_FILENO, error_message, strlen(error_message));
    write(STDERR_FILENO, "\n", 1);
  }

  // terminate
  exit(EXIT_FAILURE);
}