#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "frobenius.h"

int detect_arguments(int argc, char *argv[], char **inputFilePath, char **outputFilePath)
{
  if (argc != 5 || strcmp(argv[1], "-i") != 0 || strcmp(argv[3], "-o") != 0)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  *inputFilePath = argv[2];
  *outputFilePath = argv[4];

  return 1;
}

char *read_file(char *file_name, int *file_size)
{

  int file_descriptor = 0;
  char *file_content = NULL;
  int file_size_int = 0;
  int read_size = 0;
  int read_count = 0;
  int read_total = 0;
  char read_buffer[BUFFER_SIZE];

  if ((file_descriptor = open(file_name, O_RDONLY, 0)) < 0)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  file_size_int = lseek(file_descriptor, 0, SEEK_END);
  if (file_size_int < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }

  file_content = (char *)malloc(file_size_int + 1);
  if (file_content == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  if (lseek(file_descriptor, 0, SEEK_SET) < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }
  while (read_total < file_size_int)
  {
    read_size = read(file_descriptor, read_buffer, BUFFER_SIZE);
    if (read_size < 0)
    {
      GLOBAL_ERROR = FILE_READ_ERROR;
      return NULL;
    }

    read_count = 0;
    while (read_count < read_size)
    {
      file_content[read_total++] = read_buffer[read_count++];
    }
  }

  file_content[read_total] = '\0';

  *file_size = read_total;
  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

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
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  if (lseek(file_descriptor, 0, SEEK_SET) < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return -1;
  }

  while (write_total < file_size)
  {
    write_count = 0;
    while (write_count < BUFFER_SIZE && write_total < file_size)
    {
      write_buffer[write_count++] = file_content[write_total++];
    }

    write_size = write(file_descriptor, write_buffer, write_count);
    if (write_size < 0)
    {
      GLOBAL_ERROR = FILE_WRITE_ERROR;
      return -1;
    }
  }

  // close
  int close_res = close(file_descriptor);
  if (close_res < 0)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return -1;
  }

  return 0;
}

int lock_file(int file_desc)
{
  struct flock lock;
  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;

  if (fcntl(file_desc, F_SETLK, &lock) < 0)
  {
    GLOBAL_ERROR = FILE_LOCK_ERROR;
    return -1;
  }

  return 1;
  // int lock_res = lockf(file_descriptor, F_TLOCK, 0);
  // if (lock_res < 0)
  // {
  //   perror("lockf");
  //   exit(EXIT_FAILURE);
  // }
}

void print_error_and_exit(const Error error)
{
  char *error_message = NULL;
  int show_perror = 1;
  switch (error)
  {
  case INVALID_ARGUMENTS:
    error_message = "Invalid arguments";
    break;
  case FILE_OPEN_ERROR:
    error_message = "File open error";
    break;
  case FILE_WRITE_ERROR:
    error_message = "File write error";
    break;
  case FILE_READ_ERROR:
    error_message = "File read error";
    break;
  case FILE_LOCK_ERROR:
    error_message = "File lock error";
    break;
  case FILE_UNLOCK_ERROR:
    error_message = "File unlock error";
    break;
  case FILE_CLOSE_ERROR:
    error_message = "File close error";
    break;
  case FILE_SEEK_ERROR:
    error_message = "File seek error";
    break;
  default:
    error_message = "Unknown error";
    break;
  }

  if (show_perror)
  {
    perror(error_message);
  }
  else
  {
    write(STDERR_FILENO, error_message, strlen(error_message));
    write(STDERR_FILENO, "\n", 1);
  }

  // terminate
  exit(EXIT_FAILURE);
}

char **convert_to_env(const Coordinates coordinate)
{

  char **coordinate_env = (char **)malloc(sizeof(char *) * 11);
  if (coordinate_env == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  // add COORDINATE_1 = VALUE
  char *coordinate_1 = (char *)malloc(sizeof(char) * (strlen(coordinate.coordinate_1) + 1));
  if (coordinate_1 == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  strcpy(coordinate_1, coordinate.coordinate_1);
  coordinate_env[0] = coordinate_1;

  // add COORDINATE_2 = VALUE
  char *coordinate_2 = (char *)malloc(sizeof(char) * (strlen(coordinate.coordinate_2) + 1));
  if (coordinate_2 == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  strcpy(coordinate_2, coordinate.coordinate_2);
  coordinate_env[1] = coordinate_2;

  return coordinate_env;
}

Coordinates *convert_to_coordinates(char *content, int *coordinate_count)
{
  int file_size = strlen(content);
  int coordinates_count = file_size / 30;
  Coordinates *coordinates = (Coordinates *)malloc(sizeof(Coordinates) * coordinates_count);
  if (coordinates == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  // print size
  printf("file size: %d\n", file_size);
  printf("coordinates count: %d\n", coordinates_count);

  for (int i = 0; i < coordinates_count; i++)
  {
    coordinates[i].total_size = 0;
    coordinates[i].coordinate_1 = concat_3_values_ascii(content[i * 30], content[i * 30 + 1], content[i * 30 + 2]);
    coordinates[i].coordinate_2 = concat_3_values_ascii(content[i * 30 + 3], content[i * 30 + 4], content[i * 30 + 5]);
    coordinates[i].coordinate_3 = concat_3_values_ascii(content[i * 30 + 6], content[i * 30 + 7], content[i * 30 + 8]);
    coordinates[i].coordinate_4 = concat_3_values_ascii(content[i * 30 + 9], content[i * 30 + 10], content[i * 30 + 11]);
    coordinates[i].coordinate_5 = concat_3_values_ascii(content[i * 30 + 12], content[i * 30 + 13], content[i * 30 + 14]);
    coordinates[i].coordinate_6 = concat_3_values_ascii(content[i * 30 + 15], content[i * 30 + 16], content[i * 30 + 17]);
    coordinates[i].coordinate_7 = concat_3_values_ascii(content[i * 30 + 18], content[i * 30 + 19], content[i * 30 + 20]);
    coordinates[i].coordinate_8 = concat_3_values_ascii(content[i * 30 + 21], content[i * 30 + 22], content[i * 30 + 23]);
    coordinates[i].coordinate_9 = concat_3_values_ascii(content[i * 30 + 24], content[i * 30 + 25], content[i * 30 + 26]);
    coordinates[i].coordinate_10 = concat_3_values_ascii(content[i * 30 + 27], content[i * 30 + 28], content[i * 30 + 29]);
    if (GLOBAL_ERROR == INVALID_MALLOC)
    {
      return NULL;
    }
  }

  // // print all coordinates
  // for (int i = 0; i < coordinates_count; i++)
  // {
  //   printf("1: %s\n", coordinates[i].coordinate_1);
  //   printf("2: %s\n", coordinates[i].coordinate_2);
  //   printf("3: %s\n", coordinates[i].coordinate_3);
  //   printf("4: %s\n", coordinates[i].coordinate_4);
  //   printf("5: %s\n", coordinates[i].coordinate_5);
  //   printf("6: %s\n", coordinates[i].coordinate_6);
  //   printf("7: %s\n", coordinates[i].coordinate_7);
  //   printf("8: %s\n", coordinates[i].coordinate_8);
  //   printf("9: %s\n", coordinates[i].coordinate_9);
  //   printf("10: %s\n", coordinates[i].coordinate_10);
  //   printf("---------------------------\n");
  // }

  *coordinate_count = coordinates_count;
  return coordinates;
}

char *convert_to_ascii(int num)
{
  // detect how many digits
  int digits = 0;
  int temp = num;
  while (temp > 0)
  {
    temp /= 10;
    digits++;
  }

  // allocate memory
  char *ascii_num = (char *)malloc(digits + 1);
  if (ascii_num == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  // convert
  int i = 0;
  while (num > 0)
  {
    ascii_num[i++] = num % 10 + '0';
    num /= 10;
  }
  ascii_num[i] = '\0';

  // reverse
  char *ascii_num_reversed = (char *)malloc(digits + 1);
  if (ascii_num_reversed == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  int j = 0;
  for (i = digits - 1; i >= 0; i--)
  {
    ascii_num_reversed[j++] = ascii_num[i];
  }

  ascii_num_reversed[j] = '\0';

  // free memory
  free(ascii_num);

  return ascii_num_reversed;
}

char *concat_3_values_ascii(char c1, char c2, char c3)
{
  char *str1 = NULL;
  if ((str1 = convert_to_ascii((int)c1)) == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  char *str2 = NULL;
  if ((str2 = convert_to_ascii((int)c2)) == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  char *str3 = NULL;
  if ((str3 = convert_to_ascii((int)c3)) == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  int total_size = strlen(str1) + strlen(str2) + strlen(str3) + 4;
  char *str = (char *)malloc(total_size + 1);
  if (str == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  int i = 0;
  str[i++] = '(';

  for (int j = 0; j < strlen(str1); j++)
  {
    str[i++] = str1[j];
  }
  str[i++] = ',';
  for (int j = 0; j < strlen(str2); j++)
  {
    str[i++] = str2[j];
  }
  str[i++] = ',';
  for (int j = 0; j < strlen(str3); j++)
  {
    str[i++] = str3[j];
  }

  str[i++] = ')';
  str[i] = '\0';

  free(str1);
  free(str2);
  free(str3);

  return str;
}

void free_coordinates(Coordinates *coordinates, int coordinate_count)
{
  for (int i = 0; i < coordinate_count; i++)
  {
    free(coordinates[i].coordinate_1);
    free(coordinates[i].coordinate_2);
    free(coordinates[i].coordinate_3);
    free(coordinates[i].coordinate_4);
    free(coordinates[i].coordinate_5);
    free(coordinates[i].coordinate_6);
    free(coordinates[i].coordinate_7);
    free(coordinates[i].coordinate_8);
    free(coordinates[i].coordinate_9);
    free(coordinates[i].coordinate_10);
  }

  free(coordinates);
}