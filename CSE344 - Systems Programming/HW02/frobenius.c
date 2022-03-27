#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "frobenius.h"

int open_file(char *file_name)
{
  int file_descriptor = open(file_name, O_RDONLY, 0);
  if (file_descriptor < 0)
    return FILE_OPEN_ERROR;

  return file_descriptor;
}

char *read_file(int file_descriptor, int *file_size)
{
  char *file_content = NULL;
  int file_size_int = 0;
  int read_size = 0;
  int read_count = 0;
  int read_total = 0;
  char read_buffer[BUFFER_SIZE];

  file_size_int = lseek(file_descriptor, 0, SEEK_END);
  if (file_size_int < 0)
    return NULL;

  file_content = (char *)malloc(file_size_int + 1);
  if (file_content == NULL)
    return NULL;

  lseek(file_descriptor, 0, SEEK_SET);
  while (read_total < file_size_int)
  {
    read_size = read(file_descriptor, read_buffer, BUFFER_SIZE);
    if (read_size < 0)
      return NULL;

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
    return NULL;

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
    return FILE_WRITE_ERROR;

  lseek(file_descriptor, 0, SEEK_SET);
  while (write_total < file_size)
  {
    write_count = 0;
    while (write_count < BUFFER_SIZE && write_total < file_size)
    {
      write_buffer[write_count++] = file_content[write_total++];
    }

    write_size = write(file_descriptor, write_buffer, write_count);
    if (write_size < 0)
      return FILE_WRITE_ERROR;
  }

  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
    return FILE_WRITE_ERROR;

  return 0;
}

void lock_file(int file_desc)
{
  struct flock lock;
  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;

  fcntl(file_desc, F_SETLKW, &lock);

  // int lock_res = lockf(file_descriptor, F_TLOCK, 0);
  // if (lock_res < 0)
  // {
  //   perror("lockf");
  //   exit(EXIT_FAILURE);
  // }
}