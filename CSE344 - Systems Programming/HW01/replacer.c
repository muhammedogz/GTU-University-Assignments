#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "replacer.h"

int detect_arguments(int argc, char *argv[], ReplacePattern **pattern_arr, char **file_name)
{
  if (argc != 3)
    return INVALID_ARGUMENTS;

  char *replace_pattern = argv[1];
  *file_name = argv[2];

  // create ReplacePattern array
  int pattern_count = 0;
  *pattern_arr = initialize_replace_patterns(replace_pattern, &pattern_count);
  if (*pattern_arr == NULL)
    return INVALID_MALLOC;

  int detect_result = detect_replace_pattern(*pattern_arr, pattern_count, replace_pattern);
  if (detect_result != 0)
    return detect_result;

  if (*pattern_arr == NULL || file_name == NULL)
    return INVALID_INITIALIZATION;

  return pattern_count;
}

ReplacePattern *initialize_replace_patterns(const char *pattern, int *pattern_count)
{
  int count = 1;
  for (size_t i = 0; i < strlen(pattern); i++)
  {
    if (pattern[i] == ';')
    {
      count++;
    }
  }

  ReplacePattern *pattern_arr = (ReplacePattern *)malloc(sizeof(ReplacePattern) * count);
  if (pattern_arr == NULL)
    return NULL;

  // initialize pattern_arr
  for (int i = 0; i < count; i++)
  {
    pattern_arr[i].replace = NULL;
    pattern_arr[i].with = NULL;
    pattern_arr[i].case_sensitive = 0;
    pattern_arr[i].match_multiple = 0;
    pattern_arr[i].match_multiple_str = NULL;
    pattern_arr[i].match_beginning = 0;
    pattern_arr[i].match_end = 0;
    pattern_arr[i].match_any = 0;
    pattern_arr[i].match_any_str = NULL;
  }
  *pattern_count = count;
  return pattern_arr;
}

int detect_replace_pattern(ReplacePattern *pattern_arr, const int pattern_count, const char *pattern)
{
  const size_t size = strlen(pattern);
  int last_index = 0;
  for (int i = 0; i < pattern_count; i++)
  {
    int slash_count = 0;
    int replace_str_start = 0, replace_str_end = 0;
    int with_str_start = 0, with_str_end = 0;
    int match_multiple_start = 0;

    for (size_t j = last_index; j < size; j++)
    {
      if (pattern[j] == '/')
        slash_count_helper(&slash_count, &replace_str_start, &replace_str_end, &with_str_start, &with_str_end, j);
      if (pattern[j] == 'i')
        pattern_arr[i].case_sensitive = 1;

      if (pattern[j] == '[')
      {
        if (slash_count != 1)
          return INVALID_MATCH_MULTIPLE;
        match_multiple_start = j + 1;
      }

      if (pattern[j] == ']' && slash_count == 1)
      {
        if (slash_count != 1)
          return INVALID_MATCH_MULTIPLE;
        pattern_arr[i].match_multiple = 1;
        char *temp_str = str_initializer(pattern, match_multiple_start, j);
        if (temp_str != NULL)
          pattern_arr[i].match_multiple_str = temp_str;
        else
          return INVALID_MALLOC;
      }

      if (pattern[j] == '^')
      {
        if (slash_count != 1 || pattern[j - 1] != '/')
          return INVALID_MATCH_BEGINNING;
        replace_str_start += 1;
        pattern_arr[i].match_beginning = 1;
      }

      if (pattern[j] == '$')
      {
        if (slash_count != 1 || pattern[j + 1] != '/')
          return INVALID_MATCH_END;
        replace_str_end -= 1;
        pattern_arr[i].match_end = 1;
      }

      if (pattern[j] == '*')
      {
        if (slash_count != 1 || pattern[j - 1] == '/')
          return INVALID_MATCH_ANY;
        pattern_arr[i].match_any = 1;
        if (pattern[j - 1] == ']')
          pattern_arr[i].match_any_str = pattern_arr[i].match_multiple_str;
        else
        {
          char *str_temp = str_initializer(pattern, j - 1, j);
          if (str_temp != NULL)
            pattern_arr[i].match_any_str = str_temp;
          else
            return INVALID_MALLOC;
        }
      }

      if (pattern[j] == ';')
      {
        last_index = j + 1;
        break;
      }
    }

    if (slash_count != 3)
      return INVALID_SLASH_COUNT;
    if (replace_str_start == 0 || replace_str_end == 0 || with_str_start == 0 || with_str_end == 0)
      return INVALID_WORD_USAGE;

    // initialize replace str and with str
    char *temp_replace = str_initializer(pattern, replace_str_start, replace_str_end);
    if (temp_replace != NULL)
      pattern_arr[i].replace = temp_replace;
    else
      return INVALID_MALLOC;

    char *temp_with = str_initializer(pattern, with_str_start, with_str_end);
    if (temp_with != NULL)
      pattern_arr[i].with = temp_with;
    else
      return INVALID_MALLOC;
  }

  return 0;
}

void slash_count_helper(int *slash_count, int *replace_str_start, int *replace_str_end, int *with_str_start, int *with_str_end, const int j)
{
  *slash_count += 1;
  if (*slash_count == 1)
  {
    *replace_str_start += j + 1;
  }
  if (*slash_count == 2)
  {
    *replace_str_end += j;
    *with_str_start += j + 1;
  }
  if (*slash_count == 3)
  {
    *with_str_end += j;
  }
}

char *str_initializer(const char *str, const int start, const int end)
{
  char *new_str = (char *)malloc(sizeof(char) * (end - start + 1));
  if (new_str == NULL)
  {
    perror("malloc failed\n");
    return NULL;
  }
  strncpy(new_str, str + start, end - start);
  new_str[end - start] = '\0';
  return new_str;
}

void print_error_type(const Error error)
{
  switch (error)
  {
  case INVALID_INITIALIZATION:
    printf("INVALID_INITIALIZATION\n");
    break;
  case INVALID_MATCH_MULTIPLE:
    printf("INVALID_MATCH_MULTIPLE\n");
    break;
  case INVALID_MATCH_BEGINNING:
    printf("INVALID_MATCH_BEGINNING\n");
    break;
  case INVALID_MATCH_END:
    printf("INVALID_MATCH_END\n");
    break;
  case INVALID_MATCH_ANY:
    printf("INVALID_MATCH_ANY\n");
    break;
  case INVALID_SLASH_COUNT:
    printf("INVALID_SLASH_COUNT\n");
    break;
  case INVALID_WORD_USAGE:
    printf("INVALID_WORD_USAGE\n");
    break;
  case INVALID_MALLOC:
    printf("INVALID_MALLOC\n");
    break;
  default:
    break;
  }
}

void usage_invalid()
{
  printf("Usage is invalid. See the manual\n");
  usage_manual();
}

void usage_manual()
{
  printf("Usage: ./hw1 \"[replace pattern]\" inputFilePath\n"
         "Replace Pattern Examples\n"
         "Example: \"/str1/str2/\" \t\t -> Replace str2 with str1\n"
         "Example: \"/str1/str2/i\" \t -> Casesensitive\n"
         "Example: \"/str1/str2/;/str3/str4/\" -> Combine mutiple replace patterns  \n"
         "Example: \"/[zs]tr1/str2/\" \t -> Multiple character match\n"
         "Example: \"/^str1/str2/\" \t\t -> Match at the beginning of the line\n"
         "Example: \"/str1$/str2/\" \t\t -> Match at the end of the line\n"
         "Example: \"/st*r1/str2/\" \t\t -> Match any number of characters\n"
         "Also you can combine multiple search patterns\n");
}
