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

  printf("file size: %d\n", file_size);
  int coordinates_count = 0;
  Coordinates *coordinates = convert_to_coordinates(file_content, &coordinates_count);

  char **env1 = convert_to_env(coordinates[0]);
  // char **env2 = convert_to_env(coordinates[1]);

  for (int i = 0; i < 10; i++)
  {
    printf("env1[%d] = %s\n", i, env1[i]);
  }

  char *new_argv[] = {"./helper", "-i", "./frobenius.c", "-o", "./frobenius_out.c", NULL};
  execve("./helper", new_argv, env1);

  // free env1
  for (int i = 0; i < 10; i++)
  {
    free(env1[i]);
  }
  free(env1);

  // free
  free(file_content);
  free_coordinates(coordinates, coordinates_count);
}