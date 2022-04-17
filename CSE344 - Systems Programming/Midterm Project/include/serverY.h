#ifndef SERVERY_H
#define SERVERY_H

#include "common.h"

typedef enum
{
  WORKER_OF_Y,
  WORKER_OF_Z,
  FORWARD_TO_SERVER_Z,
} WorkerPrintTypes;

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

int runChildZ(const int logFileDescriptor, const int turn, const int time_v, const int poolSize, const int poolSize2);

void serverZ(const int pipeFd, const int logFileDescriptor, const int poolSize, const int poolSize2, const int time_v);

int printWorkerInfo(const int fd, const Matrix matrix, const pid_t workerID, const int i, const int poolSize, const int type);

int checkAlreadyRunning();

int removeTempPath();

void *createSharedMemoryChildY(const int poolSize);

void *createSharedMemoryChildZ(const int poolSize);

void *createSharedMemoryMatrix(const Matrix matrix);

void *createSharedMemoryMatrixData(const Matrix matrix);

void *getSharedMemoryChildY(const int poolSize);

void *getSharedMemoryChildZ(const int poolSize);

void *getSharedMemoryMatrix();

void *getSharedMemoryMatrixData(const Matrix matrix);

void exitGracefully(int status, Matrix matrix);

int writeMatrix(const char *path, const Matrix *matrix);

/**
 * @brief Inform user about invalid argument usage
 *
 */
void invalid_usage();

#endif