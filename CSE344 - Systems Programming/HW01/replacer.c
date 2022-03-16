#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "replacer.h"

int detect_arguments(int argc, char *argv[])
{
  // print arguments
  printf("argc: %d\n", argc);
  for (int i = 0; i < argc; i++)
  {
    printf("argv[%d]: %s\n", i, argv[i]);
  }

  if (argc != 3)
  {
    printf("Invalid count");
    usage_invalid();
    return -1;
  }
  char *replace_pattern = argv[1];
  char *file_name = argv[2];

  // create ReplacePattern array
  int pattern_count = 0;
  ReplacePattern *pattern_arr = initialize_replace_patterns(replace_pattern, &pattern_count);

  printf("pattern count: %d\n", pattern_count);

  detect_replace_pattern(pattern_arr, pattern_count, replace_pattern);

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
    printf("match_any_char: %c\n", pattern_arr[i].match_any_char);
  }

  return 0;
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

ReplacePattern *initialize_replace_patterns(const char *pattern, int *pattern_count)
{
  // detect ";" count in pattern
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
  {
    perror("malloc failed\n");
    return NULL;
  }
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
    pattern_arr[i].match_any_char = '\0';
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
    int replace_str_start = 0;
    int replace_str_end = 0;
    int with_str_start = 0;
    int with_str_end = 0;
    int case_sensitive = 0;
    int match_multiple = 0;
    char *match_multiple_str = NULL;
    int match_beginning = 0;
    int match_end = 0;
    int match_any = 0;
    char match_any_char = 0;
    int match_multiple_start = 0;
    int match_multiple_end = 0;
    for (size_t j = last_index; j < size; j++)
    {
      if (pattern[j] == '/')
      {
        slash_count++;
        if (slash_count == 1)
        {
          replace_str_start = j + 1;
        }
        if (slash_count == 2)
        {
          replace_str_end = j;
          with_str_start = j + 1;
        }
        if (slash_count == 3)
        {
          with_str_end = j;
        }
      }
      if (pattern[j] == 'i')
      {
        case_sensitive = 1;
      }
      if (pattern[j] == '[')
      {
        if (slash_count == 1)
          match_multiple_start = j;
        else
        {
          printf("Invalid pattern for [ symbol\n");
          return -1;
        }
      }
      if (pattern[j] == ']')
      {
        if (slash_count == 1)
        {
          match_multiple = 1;
          match_multiple_end = j;
        }
        else
        {
          printf("Invalid pattern for ] symbol\n");
          return -1;
        }
      }
      if (pattern[j] == '^')
      {
        if (slash_count == 1)
          match_beginning = 1;
        else
        {
          printf("Invalid pattern for ^ symbol\n");
          return -1;
        }
      }
      if (pattern[j] == '$')
      {
        if (pattern[j + 1] == '/')
          match_end = 1;
        else
        {
          printf("Invalid pattern for $ symbol\n");
          return -1;
        }
      }
      if (pattern[j] == '*')
      {
        if (slash_count == 1)
        {
          match_any_char = pattern[j - 1];
          match_any = 1;
        }
        else
        {
          printf("Invalid pattern for * symbol\n");
          return -1;
        }
      }
      if (pattern[j] == ';')
      {
        last_index = j + 1;
        break;
      }
    }
    if (slash_count != 3)
    {
      printf("last index: %d\n", last_index);
      printf("slash count: %d\n", slash_count);
      printf("invalid pattern for / usage\n");
      return -1;
    }
    if (replace_str_start == 0 || replace_str_end == 0 || with_str_start == 0 || with_str_end == 0)
    {
      printf("invalid pattern for words\n");
      return -1;
    }

    if (case_sensitive == 1)
      pattern_arr[i].case_sensitive = 1;
    if (match_multiple == 1)
    {
      match_multiple_str = (char *)malloc(sizeof(char) * (match_multiple_end - match_multiple_start + 1));
      if (match_multiple_str == NULL)
      {
        perror("malloc failed\n");
        return -1;
      }
      strncpy(match_multiple_str, pattern + match_multiple_start + 1, match_multiple_end - match_multiple_start - 1);
      match_multiple_str[match_multiple_end - match_multiple_start - 1] = '\0';
      pattern_arr[i].match_multiple_str = match_multiple_str;
      pattern_arr[i].match_multiple = 1;
    }
    if (match_beginning == 1)
    {
      replace_str_start += 1;
      pattern_arr[i].match_beginning = 1;
    }
    if (match_end == 1)
    {
      replace_str_end -= 1;
      pattern_arr[i].match_end = 1;
    }
    if (match_any == 1)
    {
      pattern_arr[i].match_any = 1;
      pattern_arr[i].match_any_char = match_any_char;
    }

    // initialize replace str and with str
    pattern_arr[i].replace = (char *)malloc(sizeof(char) * (replace_str_end - replace_str_start + 1));
    if (pattern_arr[i].replace == NULL)
    {
      perror("malloc failed\n");
      return -1;
    }
    strncpy(pattern_arr[i].replace, pattern + replace_str_start, replace_str_end - replace_str_start);
    pattern_arr[i].replace[replace_str_end - replace_str_start] = '\0';
    pattern_arr[i].with = (char *)malloc(sizeof(char) * (with_str_end - with_str_start + 1));
    if (pattern_arr[i].with == NULL)
    {
      perror("malloc failed\n");
      return -1;
    }
    strncpy(pattern_arr[i].with, pattern + with_str_start, with_str_end - with_str_start);
    pattern_arr[i].with[with_str_end - with_str_start] = '\0';
  }

  return 0;
}

void usage_invalid()
{
  printf("Usage is invalid. See the manual\n");
  usage_manual();
}
