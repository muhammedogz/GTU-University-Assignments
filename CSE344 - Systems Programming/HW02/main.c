#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "frobenius.h"

int main(int argc, char *argv[])
{
  int file_size = 0;
  char *input_file = NULL;
  char *output_file = NULL;
  char *file_content = NULL;

  if (detect_arguments(argc, argv, &input_file, &output_file) < 0)
    print_error_and_exit(GLOBAL_ERROR);

  printf("input file: %s\n", input_file);
  printf("output file: %s\n", output_file);

  if ((file_content = read_file(input_file, &file_size)) == NULL)
    print_error_and_exit(GLOBAL_ERROR);

  printf("file content: %s\n", file_content);

  if (write_file(output_file, file_content, file_size) < 0)
    print_error_and_exit(GLOBAL_ERROR);

  // free
  free(file_content);
}