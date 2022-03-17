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

  int line_count = 0;
  Line *lines = split_file_content(file_content, &line_count);
  if (lines == NULL)
    print_error_type(WORD_SPLIT_ERROR);

  int performed_replacements = perform_replace(pattern_arr, pattern_count, lines, line_count);
  if (performed_replacements < 0)
    print_error_type(performed_replacements);

  printf("%d replacements performed\n", performed_replacements);

  // print line
  for (int i = 0; i < line_count; i++)
  {
    for (int j = 0; j < lines[i].word_count; j++)
    {
      printf("%s", lines[i].words[j]);
    }
  }

  free_pattern_arr(pattern_arr, pattern_count);
  free_line_arr(lines, line_count);
  free(file_content);
}
