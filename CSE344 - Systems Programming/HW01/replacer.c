#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "replacer.h"

/* Buffer Size for File Content - Both read and write */
#define BUFFER_SIZE 1024

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
  int file_descriptor = open(file_name, O_RDONLY, 0);
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
  char read_buffer[BUFFER_SIZE];

  file_size_int = lseek(file_descriptor, 0, SEEK_END);
  if (file_size_int < 0)
    return NULL;

  file_content = (char *)malloc(file_size_int + 1);
  if (file_content == NULL)
    return NULL;

  lseek(file_descriptor, 0, SEEK_SET);
  while (read_total < file_size_int)
  {
    read_size = read(file_descriptor, read_buffer, BUFFER_SIZE);
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
  char write_buffer[BUFFER_SIZE];

  int file_descriptor = open(file_name, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
  if (file_descriptor < 0)
    return FILE_WRITE_ERROR;

  lseek(file_descriptor, 0, SEEK_SET);
  while (write_total < file_size)
  {
    write_count = 0;
    while (write_count < BUFFER_SIZE && write_total < file_size)
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

  word_arr = split_words(file_content, &word_count);
  if (word_arr == NULL)
    return NULL;

  int size = strlen(file_content);

  // detect line count
  for (int i = 0; i < size; i++)
  {
    if (file_content[i] == '\n')
      line_count++;
  }

  lines = (Line *)malloc(sizeof(Line) * line_count);
  if (lines == NULL)
    return NULL;

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

  free_word_arr(word_arr, word_count);

  *_line_count = line_count;

  return lines;
}

void print_error_type(const Error error)
{
  char *msg = NULL;
  switch (error)
  {
  case INVALID_CHAR_OCCURRENCE:
    msg = "INVALID_CHAR_OCCURRENCE";
    break;
  case INVALID_SLASH_COUNT:
    msg = "INVALID_SLASH_COUNT";
    break;
  case INVALID_WORD_USAGE:
    msg = "INVALID_WORD_USAGE";
    break;
  case INVALID_MATCH_MULTIPLE:
    msg = "INVALID_MATCH_MULTIPLE";
    break;
  case INVALID_MATCH_BEGINNING:
    msg = "INVALID_MATCH_BEGINNING";
    break;
  case INVALID_MATCH_END:
    msg = "INVALID_MATCH_END";
    break;
  case INVALID_MATCH_ANY:
    msg = "INVALID_MATCH_ANY";
    break;
  case INVALID_COMMA_USAGE:
    msg = "INVALID_COMMA_USAGE";
    break;
  case INVALID_REPLACE_PARAMETER:
    msg = "INVALID_REPLACE_PARAMETER";
    break;
  case INVALID_MALLOC:
    msg = "INVALID_MALLOC";
    perror("Invalid malloc\n");
    break;
  case FILE_OPEN_ERROR:
    msg = "FILE_OPEN_ERROR";
    perror("File open error\n");
    break;
  case FILE_READ_ERROR:
    msg = "FILE_READ_ERROR";
    perror("File read error\n");
    break;
  case FILE_WRITE_ERROR:
    msg = "FILE_WRITE_ERROR";
    perror("File write error\n");
    break;
  default:
    msg = "UNKNOWN_ERROR";
    break;
  }

  write(STDERR_FILENO, msg, strlen(msg));
}

int perform_replace(ReplacePattern *pattern_arr, int pattern_count, Line *lines, int line_count)
{
  int replace_performed = 0;

  for (int i = 0; i < pattern_count; i++)
  {
    char *replace = pattern_arr[i].replace;
    char *with = pattern_arr[i].with;
    int case_sensitive = pattern_arr[i].case_sensitive;
    int match_beginning = pattern_arr[i].match_beginning;
    int match_end = pattern_arr[i].match_end;

    printf("replace: %s\n", replace);
    printf("with: %s\n", with);
    printf("case sensitive: %d\n", case_sensitive);
    printf("match beggining (^): %d\n", match_beginning);
    printf("match end ($) : %d\n", match_end);

    for (int j = 0; j < line_count; j++)
    {
      int word_count = match_beginning != 0 ? match_beginning : lines[j].word_count;
      int start_index = match_end != 0 ? lines[j].word_count - 1 : 0;
      for (int k = start_index; k < word_count; k++)
      {
        int compare_res = compare_strings(replace, lines[j].words[k], pattern_arr[i]);
        if (compare_res == 0)
        {
          char *temp = lines[j].words[k];
          lines[j].words[k] = (char *)malloc(sizeof(char) * (strlen(with) + 1));
          if (lines[j].words[k] == NULL)
            return INVALID_MALLOC;

          strncpy(lines[j].words[k], with, strlen(with));
          lines[j].words[k][strlen(with)] = '\0';
          int enchanted_len = enchanted_strlen(temp);
          if (temp[enchanted_len - 1] == '\n' || temp[enchanted_len - 1] == ' ')
            lines[j].words[k][strlen(with)] = temp[enchanted_len - 1];

          free(temp);

          replace_performed++;
        }
        if (compare_res < 0)
          return INVALID_MALLOC;
      }
    }
  }

  return replace_performed;
}

char *concatanate_lines(Line *lines, int line_count, int *new_size)
{
  int size = 0;
  for (int i = 0; i < line_count; i++)
  {
    for (int j = 0; j < lines[i].word_count; j++)
    {
      size += enchanted_strlen(lines[i].words[j]);
    }
  }

  char *file_content = (char *)malloc(sizeof(char) * (size + 1));
  if (file_content == NULL)
    return NULL;

  int file_content_index = 0;
  for (int i = 0; i < line_count; i++)
  {
    for (int j = 0; j < lines[i].word_count; j++)
    {
      strncpy(file_content + file_content_index, lines[i].words[j], enchanted_strlen(lines[i].words[j]));
      file_content_index += enchanted_strlen(lines[i].words[j]);
    }
  }

  file_content[file_content_index] = '\0';
  *new_size = size;

  return file_content;
}

/* Free Functions */

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

void free_word_arr(char **word_arr, const int word_count)
{
  if (word_arr == NULL)
    return;

  for (int i = 0; i < word_count; i++)
  {
    if (word_arr[i] != NULL)
      free(word_arr[i]);
  }
  free(word_arr);
}

void free_line_arr(Line *line_arr, const int line_count)
{
  if (line_arr == NULL)
    return;

  for (int i = 0; i < line_count; i++)
  {
    if (line_arr[i].words != NULL)
    {
      for (int j = 0; j < line_arr[i].word_count; j++)
      {
        if (line_arr[i].words[j] != NULL)
          free(line_arr[i].words[j]);
      }
      free(line_arr[i].words);
    }
  }
  free(line_arr);
}

/* Helper Functions */

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
      if (pattern[j] == 'i' && slash_count == 3)
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

int check_char_validity(const char ch)
{
  if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '$' || ch == '^' || ch == '*' || ch == '[' || ch == ']' || ch == ';' || ch == '/')
    return 0;
  return INVALID_CHAR_OCCURRENCE;
}

char **split_words(char *file_content, int *_word_count)
{
  char **word_arr = NULL;
  int word_count = 1;

  int size = strlen(file_content);

  // detect word count
  for (int i = 0; i < size; i++)
  {
    if (file_content[i] == ' ' || file_content[i] == '\n')
      word_count++;
  }

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

  *_word_count = word_count;

  return word_arr;
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

int compare_strings(char *_str1, char *_str2, const ReplacePattern pattern)
{
  int case_sensitive = pattern.case_sensitive;
  int return_val = 0;
  char *str1 = _str1;
  char *str2 = _str2;
  // detect smaller strlen
  int size1 = enchanted_strlen(_str1);
  int size2 = enchanted_strlen(_str2);
  int size = size1 < size2 ? size1 : size2;
  int str1_index = 0;
  int str2_index = 0;

  if (case_sensitive == 1)
  {
    str1 = convert_to_lowercase(_str1);
    if (str1 == NULL)
      return INVALID_MALLOC;
    str2 = convert_to_lowercase(_str2);
    if (str2 == NULL)
      return INVALID_MALLOC;
  }

  for (int i = 0; i < size; i++)
  {
    if (str1[str1_index] == '[')
    {
      char *multiple_str = pattern.match_multiple_str;
      for (size_t j = 0; j < strlen(multiple_str); j++)
      {
        printf("multi char: %c \n", multiple_str[j]);
        printf("str2 char: %c, str2: %s \n", str2[str2_index], str2);
        if (multiple_str[j] == str2[str2_index])
        {
          str2_index++;
          return_val = 0;
          break;
        }
        else
          return_val = 1;
      }
      str1_index += strlen(multiple_str) + 1;
    }

    if (str1[str1_index] == ']' || str1[str1_index] == '*')
    {
      str1_index++;
      continue;
    }

    if (str1[str1_index] == '\0' || str2[str2_index] == '\0' || str1[str1_index] == '\n' || str2[str2_index] == '\n')
      break;

    if (str1[str1_index] != str2[str2_index])
    {
      return_val = 1;
      break;
    }

    str1_index++;
    str2_index++;
  }

  // if case sensitive == 1, then free
  if (case_sensitive == 1)
  {
    free(str1);
    free(str2);
  }

  return return_val;
}

char *convert_to_lowercase(const char *str)
{
  int size = enchanted_strlen(str);
  char *new_str = (char *)malloc(sizeof(char) * (size + 1));
  if (new_str == NULL)
  {
    perror("malloc failed\n");
    return NULL;
  }

  for (int i = 0; i < size; i++)
  {
    if (str[i] >= 'A' && str[i] <= 'Z')
      new_str[i] = str[i] + 32;
    else
      new_str[i] = str[i];
  }
  new_str[size] = '\0';

  return new_str;
}

int enchanted_strlen(const char *str)
{
  int size = 0;
  for (int i = 0; str[i] != '\0'; i++)
  {
    if (str[i] == '\n')
    {
      size++;
      break;
    }
    size++;
  }
  return size;
}

void print_pattern_arr(ReplacePattern *pattern_arr, const int pattern_count)
{
  for (int i = 0; i < pattern_count; i++)
  {
    printf("replace: %s\n", pattern_arr[i].replace);
    printf("witch: %s\n", pattern_arr[i].with);
    printf("is exit case sensitive: %d\n", pattern_arr[i].case_sensitive);
    printf("is exit match multiple: %d\n", pattern_arr[i].match_multiple);
    printf("is exit match multiple str: %s\n", pattern_arr[i].match_multiple_str);
    printf("is exit match beginning (^): %d\n", pattern_arr[i].match_beginning);
    printf("is exit match end ($): %d\n", pattern_arr[i].match_end);
    printf("is exit match any (*): %d\n", pattern_arr[i].match_any);
    printf("is exit match any str: %s\n", pattern_arr[i].match_any_str);
    printf("-------------------\n");
  }
}
