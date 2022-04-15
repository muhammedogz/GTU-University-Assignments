#ifndef CLIENT_H
#define CLIENT_H

#include "common.h"

/**
 * @brief Check if the given arguemtns are valid or not
 *
 * @param argc Argument count
 * @param argv Argument vector
 * @param pathToServerFifo Path to server fifo
 * @param pathToDataFile Path to data file
 * @return int 1 if arguments are valid, error otherwise
 */
int detectArguments(int argc, char *argv[], char **pathToServerFifo, char **pathToDataFile);

/**
 * @brief Read the file and return its content
 *
 * @param fileName File name
 * @param fileSize File size
 * @return char* File content
 */
char *readFile(char *fileName, int *fileSize);

/**
 * @brief Convert given content to matrix due to ","
 *
 * @param content Content to convert
 * @param contentSize Content size
 * @return Matrix* Matrix
 */
Matrix *convertToMatrix(const char *content, const int contentSize);

/**
 * @brief Write given matrix to given path
 *
 * @param path Path to write
 * @param matrix Matrix to write
 * @return int 0 if success, error otherwise
 */
int writeMatrix(const char *path, const Matrix *matrix);

/**
 * @brief Detect if matrix is inverrtible or not with reading from fifo
 *
 * @param file path for fifo
 * @return int 1 if matrix is inververtible, 0 if not, error otherwise
 */
int detectInvertible(const char *file);

/**
 * @brief free given variables and exit
 *
 * @param content Content to free
 * @param matrix Matrix to free
 * @param exit_status Exit status
 */
void freeAndExit(char *content, Matrix *matrix, int exit_status);

/**
 * @brief Write to stderr the usage of the program
 *
 */
void invalidUsage();

#endif