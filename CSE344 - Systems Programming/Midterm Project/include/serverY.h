#ifndef SERVERY_H
#define SERVERY_H

#include "common.h"

#define TEMP_PATH "serverYTemp"

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

Matrix readFromPipe(const int pipeFd);

int writeToPipe(const int pipeFd, const Matrix *matrix);

int runChildY(const int closePipe, const int readPipe, const int logFileDescriptor, const int turn, const int time_v, const int poolSize, int runStatus);

int printWorkerInfo(const int fd, const Matrix matrix, const pid_t workerID, const int i, const int poolSize);

int checkAlreadyRunning();

int removeTempPath();

void *createSharedMemoryChildY(const int poolSize);

void *getSharedMemoryChildY(const int poolSize);

void exitGracefully(int status, Matrix matrix);

/**
 * @brief Inform user about invalid argument usage
 *
 */
void invalid_usage();

#endif