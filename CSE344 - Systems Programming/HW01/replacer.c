#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "replacer.h"

#define READ_BUFFER_SIZE 1024

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

int open_file(char *file_name)
{
  int file_descriptor = open(file_name, O_RDONLY, 'r');
  if (file_descriptor < 0)
    return FILE_OPEN_ERROR;

  return file_descriptor;
}

char *read_file(int file_descriptor, int *file_size)
{
  char *file_content = NULL;
  int file_size_int = 0;
  int read_size = 0;
  int read_count = 0;
  int read_total = 0;
  char read_buffer[READ_BUFFER_SIZE];

  file_size_int = lseek(file_descriptor, 0, SEEK_END);
  if (file_size_int < 0)
    return NULL;

  file_content = (char *)malloc(file_size_int + 1);
  if (file_content == NULL)
    return NULL;

  lseek(file_descriptor, 0, SEEK_SET);
  while (read_total < file_size_int)
  {
    read_size = read(file_descriptor, read_buffer, READ_BUFFER_SIZE);
    if (read_size < 0)
      return NULL;

    read_count = 0;
    while (read_count < read_size)
    {
      file_content[read_total] = read_buffer[read_count];
      read_total++;
      read_count++;
    }
  }

  file_content[read_total] = '\0';

  *file_size = read_total;
  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
    return NULL;

  return file_content;
}

int write_file(char *file_name, char *file_content, int file_size)
{
  int write_size = 0;
  int write_count = 0;
  int write_total = 0;
  char write_buffer[READ_BUFFER_SIZE];

  int file_descriptor = open(file_name, O_WRONLY | O_CREAT | O_TRUNC, 'w');

  lseek(file_descriptor, 0, SEEK_SET);
  while (write_total < file_size)
  {
    write_count = 0;
    while (write_count < READ_BUFFER_SIZE && write_total < file_size)
    {
      write_buffer[write_count] = file_content[write_total];
      write_total++;
      write_count++;
    }

    write_size = write(file_descriptor, write_buffer, write_count);
    if (write_size < 0)
      return FILE_WRITE_ERROR;
  }

  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
    return FILE_WRITE_ERROR;

  return 0;
}

void lock_file(int file_desc)
{
  struct flock lock;
  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;

  fcntl(file_desc, F_SETLKW, &lock);

  // int lock_res = lockf(file_descriptor, F_TLOCK, 0);
  // if (lock_res < 0)
  // {
  //   perror("lockf");
  //   exit(EXIT_FAILURE);
  // }
}

Line *split_file_content(char *file_content, int *_line_count)
{
  Line *lines = NULL;
  int line_count = 1;

  char **word_arr = NULL;
  int word_count = 1;

  int size = strlen(file_content);

  // detect word count
  for (int i = 0; i < size; i++)
  {
    if (file_content[i] == ' ' || file_content[i] == '\n')
      word_count++;
  }

  // detect line count
  for (int i = 0; i < size; i++)
  {
    if (file_content[i] == '\n')
      line_count++;
  }

  lines = (Line *)malloc(sizeof(Line) * line_count);
  if (lines == NULL)
    return NULL;

  word_arr = (char **)malloc(sizeof(char *) * word_count);
  if (word_arr == NULL)
    return NULL;

  int word_index = 0;
  int word_start = 0;
  int word_end = 0;

  for (int i = 0; i < size; i++)
  {
    if (file_content[i] == ' ' || file_content[i] == '\n' || i == size - 1)
    {
      word_end = i + 1;
      word_arr[word_index] = (char *)malloc(sizeof(char) * (word_end - word_start + 1));
      if (word_arr[word_index] == NULL)
        return NULL;

      strncpy(word_arr[word_index], file_content + word_start, word_end - word_start);
      word_arr[word_index][word_end - word_start] = '\0';

      word_index++;
      word_start = i + 1;
    }
  }

  int line_index = 0;
  int line_start = 0;
  int line_end = 0;

  for (int i = 0; i < word_count; i++)
  {
    if (word_arr[i][strlen(word_arr[i]) - 1] == '\n' || i == word_count - 1)
    {
      line_end = i + 1;
      lines[line_index].words = (char **)malloc(sizeof(char *) * (line_end - line_start + 1));
      if (lines[line_index].words == NULL)
        return NULL;

      lines[line_index].word_count = line_end - line_start;
      int line_word_index = 0;
      for (int j = line_start; j < line_end; j++)
      {
        lines[line_index].words[line_word_index] = (char *)malloc(sizeof(char) * (strlen(word_arr[j]) + 1));
        if (lines[line_index].words[line_word_index] == NULL)
          return NULL;

        strncpy(lines[line_index].words[line_word_index], word_arr[j], strlen(word_arr[j]));
        lines[line_index].words[line_word_index][strlen(word_arr[j])] = '\0';
        line_word_index++;
      }

      line_index++;
      line_start = i + 1;
    }
  }

  // free word_arr
  for (int i = 0; i < word_count; i++)
  {
    free(word_arr[i]);
  }
  free(word_arr);

  *_line_count = line_count;

  return lines;
}

void free_pattern_arr(ReplacePattern *pattern_arr, const int pattern_count)
{
  if (pattern_arr == NULL)
    return;

  for (int i = 0; i < pattern_count; i++)
  {
    if (pattern_arr[i].replace != NULL)
      free(pattern_arr[i].replace);
    if (pattern_arr[i].with != NULL)
      free(pattern_arr[i].with);
    if (pattern_arr[i].match_multiple_str != NULL)
      free(pattern_arr[i].match_multiple_str);
    if (pattern_arr[i].match_any_str != NULL)
      free(pattern_arr[i].match_any_str);
  }
  free(pattern_arr);
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
      if (check_char_validity(pattern[j]) != 0)
        return INVALID_CHAR_OCCURRENCE;
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
        {
          pattern_arr[i].match_any_str = malloc(sizeof(char) * (strlen(pattern_arr[i].match_multiple_str) + 1));
          if (pattern_arr[i].match_any_str == NULL)
            return INVALID_MALLOC;
          strcpy(pattern_arr[i].match_any_str, pattern_arr[i].match_multiple_str);
        }
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
        if (pattern[j + 1] != '/')
          return INVALID_COMMA_USAGE;
        last_index = j + 1;
        break;
      }

      if (slash_count == 0)
        return INVALID_REPLACE_PARAMETER;
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
  case INVALID_CHAR_OCCURRENCE:
    printf("Invalid character occurrence\n");
    break;
  case INVALID_SLASH_COUNT:
    printf("Invalid slash count\n");
    break;
  case INVALID_WORD_USAGE:
    printf("Invalid word usage\n");
    break;
  case INVALID_MATCH_MULTIPLE:
    printf("Invalid match multiple\n");
    break;
  case INVALID_MATCH_BEGINNING:
    printf("Invalid match beginning\n");
    break;
  case INVALID_MATCH_END:
    printf("Invalid match end\n");
    break;
  case INVALID_MATCH_ANY:
    printf("Invalid match any\n");
    break;
  case INVALID_COMMA_USAGE:
    printf("Invalid comma usage\n");
    break;
  case INVALID_REPLACE_PARAMETER:
    printf("Invalid replace parameter\n");
    break;
  case INVALID_MALLOC:
    perror("Invalid malloc\n");
    break;
  case FILE_OPEN_ERROR:
    perror("File open error\n");
    break;
  case FILE_READ_ERROR:
    perror("File read error\n");
    break;
  case FILE_WRITE_ERROR:
    perror("File write error\n");
    break;
  default:
    printf("Unknown error\n");
    break;
  }
}

int check_char_validity(const char ch)
{
  if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '$' || ch == '^' || ch == '*' || ch == '[' || ch == ']' || ch == ';' || ch == '/')
    return 0;
  return INVALID_CHAR_OCCURRENCE;
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
