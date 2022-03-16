#ifndef REPLACER_H
#define REPLACER_H

typedef struct
{
  char *replace;
  char *with;
  int case_sensitive;
  int match_beginning;
  int match_end;
  int match_any;
  int match_multiple;
} ReplacePattern;

int detect_arguments(int argc, char *argv[]);
int open_file(char *file_name);

// helper functions
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

void detect_replace_pattern(ReplacePattern *replace_pattern, const char *pattern);

#endif // REPLACER_H