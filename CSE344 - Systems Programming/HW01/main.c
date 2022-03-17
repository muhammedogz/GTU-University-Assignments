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

  // If you want to how replacement works, uncomment the following line
  // print_pattern_arr(pattern_arr, pattern_count);

  file_descriptor = open_file(file_name);
  if (file_descriptor < 0)
    print_error_type(file_descriptor);

  file_content = read_file(file_descriptor, &file_size);
  if (file_content == NULL)
    print_error_type(FILE_READ_ERROR);

  lock_file(file_descriptor);

  int line_count = 0;
  Line *lines = split_file_content(file_content, &line_count);
  if (lines == NULL)
    print_error_type(WORD_SPLIT_ERROR);

  int performed_replacements = perform_replace(pattern_arr, pattern_count, lines, line_count);
  if (performed_replacements < 0)
    print_error_type(performed_replacements);

  // printf("%d replacements performed\n", performed_replacements);

  int new_size = 0;
  char *new_file_content = concatanate_lines(lines, line_count, &new_size);
  if (new_file_content == NULL)
  {
    print_error_type(1);
    // print_error_type("FILE_CONCAT_ERROR");
  }

  printf("new_file_content: \n%s\n", new_file_content);
  // printf("new_size: %d\n", new_size);

  int write_result = write_file("ass.txt", new_file_content, new_size);
  if (write_result < 0)
    print_error_type(write_result);

  // // print line
  // for (int i = 0; i < line_count; i++)
  // {
  //   for (int j = 0; j < lines[i].word_count; j++)
  //   {
  //     printf("%s", lines[i].words[j]);
  //   }
  // }

  free(file_content);
  free(new_file_content);
  free_pattern_arr(pattern_arr, pattern_count);
  free_line_arr(lines, line_count);
}
