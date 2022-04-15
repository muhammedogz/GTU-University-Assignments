#ifndef SERVERY_H
#define SERVERY_H

/**
 * @brief Detect arguments and assign to variables
 *
 * @param argc Argument count
 * @param argv Argument vector
 * @param pathToServerFifo Path to server fifo
 * @param pathToLogFile Path to log file
 * @param poolSize Pool size of ServerY
 * @param poolSize2 Pool size of ServerZ
 * @param time Timeout
 * @return int 1 if arguments are valid, error otherwise
 */
int detectArguments(int argc, char *argv[], char **pathToServerFifo, char **pathToLogFile, int *poolSize, int *poolSize2, int *time);

Matrix readMatrix(const char *file);

/**
 * @brief Inform user about invalid argument usage
 *
 */
void invalid_usage();

#endif