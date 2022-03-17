#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "replacer.h"

int main(int argc, char *argv[])
{
  ReplacePattern *pattern_arr = NULL;
  char *file_name = NULL;
  int file_size = 0;
  int file_descriptor = 0;
  char *file_content = NULL;

  int pattern_count = detect_arguments(argc, argv, &pattern_arr, &file_name);
  if (pattern_count < 0)
    print_error_type(pattern_count);

  file_descriptor = open_file(file_name);
  if (file_descriptor < 0)
    print_error_type(file_descriptor);

  file_content = read_file(file_descriptor, &file_size);
  if (file_content == NULL)
    print_error_type(FILE_READ_ERROR);

  lock_file(file_descriptor);

  char *new_content = "pouet";
  int new_content_size = strlen(new_content);
  file_descriptor = open_file(file_name);

  if (file_descriptor < 0)
    print_error_type(FILE_OPEN_ERROR);
  int write_result = write_file(file_name, new_content, new_content_size);
  if (write_result < 0)
    print_error_type(write_result);

  printf("File Size: %d\n", file_size);
  // printf("File Content: %s\n", file_content);

  // // print ReplacePattern array
  // for (int i = 0; i < pattern_count; i++)
  // {
  //   printf("replace: %s\n", pattern_arr[i].replace);
  //   printf("with: %s\n", pattern_arr[i].with);
  //   printf("case_sensitive: %d\n", pattern_arr[i].case_sensitive);
  //   printf("match_multiple: %d\n", pattern_arr[i].match_multiple);
  //   printf("match_multiple_str: %s\n", pattern_arr[i].match_multiple_str);
  //   printf("match_beginning: %d\n", pattern_arr[i].match_beginning);
  //   printf("match_end: %d\n", pattern_arr[i].match_end);
  //   printf("match_any: %d\n", pattern_arr[i].match_any);
  //   printf("match_any_str: %s\n", pattern_arr[i].match_any_str);
  // }

  free_pattern_arr(pattern_arr, pattern_count);
  free(file_content);
}
