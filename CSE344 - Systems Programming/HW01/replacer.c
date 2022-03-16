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
  ReplacePattern *replace_patterns = initialize_replace_patterns(replace_pattern, &pattern_count);

  printf("hit\n");
  printf("pattern count: %d\n", pattern_count);

  // print ReplacePattern array
  for (int i = 0; i < pattern_count; i++)
  {
    printf("replace: %s\n", replace_patterns[i].replace);
    printf("with: %s\n", replace_patterns[i].with);
    printf("case_sensitive: %d\n", replace_patterns[i].case_sensitive);
  }

  return 0;
}

void usage_manual()
{
  printf("Usage: ./hw1 \"[replace pattern]\" inputFilePath\n"
         "Replace Pattern Examples\n"
         "Example: \"/str1/str2/\" \t\t -> Replace str2 with str1\n"
         "Example: \"/str1/str2/i\" \t -> Casesensitive\n"
         "Example: \"/str1/str2/;/str3/str4/\" -> Combine mutliple replace patterns  \n"
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

  ReplacePattern *replace_patterns = (ReplacePattern *)malloc(sizeof(ReplacePattern) * count);
  if (replace_patterns == NULL)
  {
    perror("malloc failed\n");
    return NULL;
  }
  // initialize replace_patterns
  for (int i = 0; i < count; i++)
  {
    replace_patterns[i].replace = NULL;
    replace_patterns[i].with = NULL;
    replace_patterns[i].case_sensitive = 0;
    replace_patterns[i].match_beginning = 0;
    replace_patterns[i].match_end = 0;
    replace_patterns[i].match_any = 0;
    replace_patterns[i].match_multiple = 0;
  }
  *pattern_count = count;
  return replace_patterns;
}

void detect_replace_pattern(ReplacePattern* replace_patterns, const char* pattern)
{
  int count = 0;
  const size_t size = strlen(pattern);
  int last_index = 0;
  for (int i = 0; i < count; i++)
  {
    int slash_count = 0;
    for (size_t j = last_index; j < size; j++)
    {
      if (pattern[j] == '/')
      {
        slash_count++;
      }
    }
  }
}

void usage_invalid()
{
  printf("Usage is invalid. See the manual\n");
  usage_manual();
}
