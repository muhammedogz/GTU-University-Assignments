#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "replacer.h"

int main(int argc, char *argv[])
{
  ReplacePattern *pattern_arr = NULL;
  char *file_name = NULL;
  int pattern_count = detect_arguments(argc, argv, &pattern_arr, &file_name);
  if (pattern_count < 0)
  {
    printf("Error: %d\n", pattern_count);
    print_error_type(pattern_count);
  }
  printf("pattern_count: %d\n", pattern_count);

  // print ReplacePattern array
  for (int i = 0; i < pattern_count; i++)
  {
    printf("replace: %s\n", pattern_arr[i].replace);
    printf("with: %s\n", pattern_arr[i].with);
    printf("case_sensitive: %d\n", pattern_arr[i].case_sensitive);
    printf("match_multiple: %d\n", pattern_arr[i].match_multiple);
    printf("match_multiple_str: %s\n", pattern_arr[i].match_multiple_str);
    printf("match_beginning: %d\n", pattern_arr[i].match_beginning);
    printf("match_end: %d\n", pattern_arr[i].match_end);
    printf("match_any: %d\n", pattern_arr[i].match_any);
    printf("match_any_str: %s\n", pattern_arr[i].match_any_str);
  }

  // print file_name
  printf("file_name: %s\n", file_name);
}