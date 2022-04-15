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

/**
 * @brief Read matrix from given path
 *
 * @param file Path to read
 * @return Matrix, if matrix is not valid, it will have null data
 */
Matrix readMatrix(const char *file);

/**
 * @brief Write matrix invertible info to client fifo
 *
 * @param clientFifo Client fifo path
 * @param invertible Invertible info
 * @return int 1 if success, error otherwise
 */
int writeToClientFifo(const char *clientFifo, const int invertible);

/**
 * @brief Inform user about invalid argument usage
 *
 */
void invalid_usage();

#endif