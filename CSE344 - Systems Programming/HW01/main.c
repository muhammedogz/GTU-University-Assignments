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

  int word_count = 0;
  char **word_arr = split_file_content(file_content, &word_count);
  if (word_arr == NULL)
    print_error_type(WORD_SPLIT_ERROR);

  printf("last char of file_content: %c\n", file_content[file_size - 1]);

  // print words
  printf("word count: %d\n", word_count);
  for (int i = 0; i < word_count; i++)
    printf("%s", word_arr[i]);

  free_pattern_arr(pattern_arr, pattern_count);
  free(file_content);
  // free word_arr
  for (int i = 0; i < word_count; i++)
    free(word_arr[i]);
  free(word_arr);
}
