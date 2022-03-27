#ifndef FROBENIUS_H
#define FROBENIUS_H

typedef struct {
  char* coordinate_1;
  char* coordinate_2;
  char* coordinate_3;
  char* coordinate_4;
  char* coordinate_5;
  char* coordinate_6;
  char* coordinate_7;
  char* coordinate_8;
  char* coordinate_9;
  char* coordinate_10;
} Coordinates;

typedef enum {
  INVALID_MALLOC = -1,
  FILE_OPEN_ERROR = -2,
  FILE_READ_ERROR = -3,
  FILE_WRITE_ERROR = -4,
  INVALID_ARGUMENTS = -5,
} Error;

/* Main Functions */
/* This functions are called from main */

/**
 * @brief open file and return file descriptor
 *
 * @param file_name File name
 * @return int file descriptor or negative error code
 */
int open_file(char *file_name);

/**
 * @brief read file and return file content
 *
 * @param file_desc File descriptor
 * @param file_size Pointer to the file size
 * @return char* file content or NULL
 */
char *read_file(int file_desc, int *file_size);

/**
 * @brief write file
 *
 * @param file_desc File descriptor
 * @param file_content File content
 * @param file_size File size
 * @return int 0 or negative error code
 */
int write_file(char *file_name, char *file_content, int file_size);

#endif