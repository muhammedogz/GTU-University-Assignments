#include <stdio.h>
#include <unistd.h>
#include "frobenius.h"

#define FILE_NAME "test.txt"

int main()
{
  int file_descriptor = 0;
  int file_size = 0;
  char *file_content = NULL;

  file_descriptor = open_file(FILE_NAME);

  file_content = read_file(file_descriptor, &file_size);

  printf("file content: %s\n", file_content);
}