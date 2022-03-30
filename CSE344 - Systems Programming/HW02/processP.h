#ifndef PROCESSP_H
#define PROCESSP_H

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
  int total_size;
} Coordinates;

typedef enum
{
  INVALID_MALLOC = 1,
  FILE_OPEN_ERROR,
  FILE_READ_ERROR,
  FILE_WRITE_ERROR,
  FILE_LOCK_ERROR,
  FILE_UNLOCK_ERROR,
  FILE_CLOSE_ERROR,
  FILE_SEEK_ERROR,
  INVALID_ARGUMENTS,
  INVALID_EXECVE,
  INVALID_FORK,
  INVALID_WAIT,

  //
  INVALID_EXIT_STATUS,
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

/**
 * @brief Convert given content to correspending coordinates due to their ascii number values
 *
 * @param content Content to convert
 * @return Coordinates* Pointer to the coordinates array
 */
Coordinates *convert_to_coordinates(char *content, int *coordinates_count);

/**
 * @brief Get corraspending environment variables from the given coordinates and convert it to environment variables
 *
 * @param coordinate Coordinates to convert
 * @return char** Pointer to the environment variables array
 */
char **convert_to_env(const Coordinates coordinate);

/**
 * @brief Call child processes to execute the calculate matrix
 *
 * @param output_file Output file path
 * @param coordinates Coordinates to calculate
 * @param coordinates_count Coordinates count
 * @return int 0 on success or error code on fail
 */
int run_child_process(char *output_file, const Coordinates *coordinates, const int coordinates_count);

/* free functions */

/**
 * @brief Free the given coordinates
 *
 * @param coordinates Coordinates to free
 * @param coordinates_count Coordinates count
 */
void free_coordinates(Coordinates *coordinates, int coordinates_count);

/**
 * @brief Free the given environment variables
 *
 * @param env Environment variables to free
 */
void free_env(char **env);

/* Helper Functions */
/* Those functions are not called from main, those are just helpers */

/**
 * @brief Get the ascii value of given character
 *
 * @param num Character to get ascii value
 * @return char* ascii value of given character as string
 */
char *convert_to_ascii(int num);

/**
 * @brief Convert those given 3 values to their ascii number respresentations
 *
 * @param c1
 * @param c2
 * @param c3
 * @return char* Concatanated version, This functions calls convert_to_ascii()
 */
char *concat_3_values_ascii(char c1, char c2, char c3);

char *int_to_string(int i);

#endif