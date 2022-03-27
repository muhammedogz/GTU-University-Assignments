#ifndef FROBENIUS_H
#define FROBENIUS_H

/* Buffer Size for File Content - Both read and write */
#define BUFFER_SIZE 1024

typedef struct
{
  char *coordinate_1;
  char *coordinate_2;
  char *coordinate_3;
  char *coordinate_4;
  char *coordinate_5;
  char *coordinate_6;
  char *coordinate_7;
  char *coordinate_8;
  char *coordinate_9;
  char *coordinate_10;
} Coordinates;

typedef enum
{
  INVALID_MALLOC,
  FILE_OPEN_ERROR,
  FILE_READ_ERROR,
  FILE_WRITE_ERROR,
  FILE_LOCK_ERROR,
  FILE_UNLOCK_ERROR,
  FILE_CLOSE_ERROR,
  FILE_SEEK_ERROR,
  INVALID_ARGUMENTS,
} Error;

// Global error type to be used in the program
Error GLOBAL_ERROR;

/* Main Functions */
/* This functions are called from main */

/**
 * @brief Detect if the given argumetns are valid or not. If valid, assign input and output file
 *
 * @param argc Argument count
 * @param argv Argument values
 * @param inputFilePath Input file path
 * @param outputFilePath Output file path
 * @return INVALID_ARGUMENTS error on fail, 1 on success
 */
int detect_arguments(int argc, char *argv[], char **inputFilePath, char **outputFilePath);

/**
 * @brief read file and return file content
 *
 * @param file_name File name to read
 * @param file_size Pointer to the file size
 * @return char* file content or NULL
 */
char *read_file(char *file_name, int *file_size);

/**
 * @brief write file
 *
 * @param file_desc File descriptor
 * @param file_content File content
 * @param file_size File size
 * @return int 0 or negative error code
 */
int write_file(char *file_name, char *file_content, int file_size);

/**
 * @brief Get an error type and print corresponding error message and terminate the program
 *
 * @param error Error type to show
 */
void print_error_and_exit(const Error error);

#endif