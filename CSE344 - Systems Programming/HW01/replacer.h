#ifndef REPLACER_H
#define REPLACER_H

typedef struct
{
  char *replace;
  char *with;
  int case_sensitive;
  int match_multiple;
  char *match_multiple_str;
  int match_beginning;
  int match_end;
  int match_any;
  char *match_any_str;
} ReplacePattern;

typedef struct
{
  char **words;
  int word_count;
} Line;

// ERROR ENUM TYPES
typedef enum
{
  INVALID_MALLOC = -1,
  INVALID_MATCH_MULTIPLE = -2,
  INVALID_MATCH_ANY = -3,
  INVALID_MATCH_BEGINNING = -4,
  INVALID_MATCH_END = -5,
  INVALID_SLASH_COUNT = -6,
  INVALID_WORD_USAGE = -7,
  INVALID_ARGUMENTS = -8,
  INVALID_INITIALIZATION = -9,
  INVALID_CHAR_OCCURRENCE = -10,
  INVALID_REPLACE_PARAMETER = -11,
  INVALID_COMMA_USAGE = -12,
  FILE_OPEN_ERROR = -13,
  FILE_READ_ERROR = -14,
  FILE_WRITE_ERROR = -15,
  WORD_SPLIT_ERROR = -16,
} Error;

/**
 * @brief Inspect arguments and return the number of ReplacePatterns
 *
 * @param argc Argument Count
 * @param argv Argument Vector
 * @param pattern_arr Pointer to the ReplacePattern array
 * @param file_name Pointer to the file name
 * @return int number of pattern or negative error code
 */
int detect_arguments(int argc, char *argv[], ReplacePattern **pattern_arr, char **file_name);

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

/**
 * @brief Lock given file descriptor
 *
 * @param file_desc File descriptor
 */
void lock_file(int file_desc);

/**
 * @brief Split string into Lines
 *
 * @param file_content File content
 * @param word_count Pointer to the word count
 * @return Line array or NULL
 */
Line *split_file_content(char *file_content, int *line_count);

/**
 * @brief Free the memory allocated for the ReplacePattern array
 *
 * @param pattern_arr Pointer to the ReplacePattern array
 * @param pattern_count Number of patterns
 */
void free_pattern_arr(ReplacePattern *pattern_arr, const int pattern_count);

// helper functions
// Those functions are used for better functionality, They are not for main.c

/**
 * @brief print usage manual
 */
void usage_manual();

/**
 * @brief print error message
 */
void usage_invalid();

/**
 * @brief Create ReplacePattern array
 *
 * @param pattern The pattern that will be used to create ReplacePattern array
 * @param pattern_count Detected pattern count
 * @return ReplacePattern* The created ReplacePattern array
 */
ReplacePattern *initialize_replace_patterns(const char *pattern, int *pattern_count);

/**
 * @brief Detect pattern and initialize ReplacePattern array
 *
 * @param pattern_arr The ReplacePattern array
 * @param pattern_count Detected pattern count
 * @param pattern The pattern that will be used to initialize ReplacePattern array
 * @return int 0 if success, negative if failed
 */
int detect_replace_pattern(ReplacePattern *pattern_arr, const int pattern_count, const char *pattern);

/**
 * @brief Count of slash in a string
 *
 * @param slash_count The count of slash
 * @param replace_str_start  The start index of replace string
 * @param replace_str_end  The end index of replace string
 * @param with_str_start  The start index of with string
 * @param with_str_end  The end index of with string
 * @param j The index of string
 */
void slash_count_helper(int *slash_count, int *replace_str_start, int *replace_str_end, int *with_str_start, int *with_str_end, const int j);

/**
 * @brief Initialize a string due to given pattern and indexes
 *
 * @param str The string that will be used
 * @param start The start index of string
 * @param end  The end index of string
 * @return char* Return the initialized string
 */
char *str_initializer(const char *str, const int start, const int end);

/**
 * @brief Print error type due to given error code
 *
 * @param error_type Given error code
 */
void print_error_type(const Error error_type);

/**
 * @brief Check if given char contains invalid character
 *
 * @param ch Given char
 * @return int 0 if valid, negative if invalid
 */
int check_char_validity(const char ch);

/**
 * @brief Split given string into words
 *
 * @param file_content File content
 * @param word_count Pointer to the word count
 * @return char** The splitted words
 */
char **split_words(char *file_content, int *word_count);

#endif // REPLACER_H