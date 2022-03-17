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
} Error;

int detect_arguments(int argc, char *argv[], ReplacePattern **pattern_arr, char **file_name);
int open_file(char *file_name);

// helper functions
// Those functions are used for better functionality, They are not for main.c

/**
 * @brief print usage manual
 */
void usage_manual();

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

void print_error_type(const Error error_type);

#endif // REPLACER_H