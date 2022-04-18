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

pid_t createServerZ(const int pipeRead, const int pipeWrite, const int fd, const int poolSize, const int poolSize2, const int time_v);

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
 * @brief Read from the given pipe and return a matrix
 *
 * @param pipeFd Pipe file descriptor
 * @return Matrix Matrix read from pipe
 */
Matrix readFromPipe(const int pipeFd);

/**
 * @brief Write matrix to given pipe
 *
 * @param pipeFd Pipe file descriptor
 * @param matrix Matrix to write
 * @return int 1 if success, error otherwise
 */
int writeToPipe(const int pipeFd, const Matrix *matrix);

/**
 * @brief Run child of (worker of) ServerY
 *
 * @param closePipe Pipe to close
 * @param readPipe Pipe to read from
 * @param logFileDescriptor Log file descriptor
 * @param turn Turn of the worker
 * @param time_v Timeout
 * @param poolSize Pool size of ServerY
 * @param runStatus Running status
 * @return int 1 if success, error otherwise
 */
int runChildY(const int closePipe, const int readPipe, const int logFileDescriptor, const int turn, const int time_v, const int poolSize, int runStatus);

/**
 * @brief Run child of (worker of) ServerZ
 *
 * @param logFileDescriptor Log file descriptor
 * @param turn Turn of the worker
 * @param time_v Timeout
 * @param poolSize Pool size of ServerY
 * @param poolSize2 Pool size of ServerZ
 * @return int 1 if success, error otherwise
 */
int runChildZ(const int logFileDescriptor, const int turn, const int time_v, const int poolSize, const int poolSize2);

/**
 * @brief Server Z process
 *
 * @param pipeFd Pipe file descriptor
 * @param logFileDescriptor Log file descriptor
 * @param poolSize Pool size of ServerY
 * @param poolSize2 Pool size of ServerZ
 * @param time_v Timeout
 */
void serverZ(const int pipeFd, const int logFileDescriptor, const int poolSize, const int poolSize2, const int time_v);

/**
 * @brief Print worker info
 *
 * @param fd Log file descriptor
 * @param matrix Matrix to print
 * @param workerID Worker ID
 * @param i Turn of the worker
 * @param poolSize Pool size of ServerY or ServerZ
 * @param type Type of worker
 */
int printWorkerInfo(const int fd, const Matrix matrix, const pid_t workerID, const int i, const int poolSize, const int type);

/**
 * @brief Checks if the server is already running
 *
 * @return int 1 if server is already running, 0 otherwise
 */
int checkAlreadyRunning();

/**
 * @brief Remove the temp path
 *
 * @return int
 */
int removeTempPath();

/**
 * @brief Create a Shared Memory Child Y object
 *
 * @param poolSize Pool size of ServerY
 * @return void* Shared Memory Child Y object
 */
void *createSharedMemoryChildY(const int poolSize);

/**
 * @brief Create a Shared Memory Child Z object
 *
 * @param poolSize Pool size of ServerZ
 * @return void* Shared Memory Child Z object
 */
void *createSharedMemoryChildZ(const int poolSize);

/**
 * @brief Create a Shared Memory Matrix object
 *
 * @param matrix  Matrix to create Shared Memory Matrix object
 * @return void* Shared Memory Matrix object
 */
void *createSharedMemoryMatrix(const Matrix matrix);

/**
 * @brief Create a Shared Memory Matrix Data object
 *
 * @param matrix Matrix to create Shared Memory Matrix Data object
 * @return void* Shared Memory Matrix Data object
 */
void *createSharedMemoryMatrixData(const Matrix matrix);

/**
 * @brief Get the Shared Memory Child Y object
 *
 * @param poolSize Pool size of ServerY
 * @return void* Shared Memory Child Y object
 */
void *getSharedMemoryChildY(const int poolSize);

/**
 * @brief Get the Shared Memory Child Z object
 *
 * @param poolSize Pool size of ServerZ
 * @return void* Shared Memory Child Z object
 */
void *getSharedMemoryChildZ(const int poolSize);

/**
 * @brief Get the Shared Memory Matrix object
 *
 * @return void* Shared Memory Matrix object
 */
void *getSharedMemoryMatrix();

/**
 * @brief Get the Shared Memory Matrix Data object
 *
 * @param matrix Matrix to get Shared Memory Matrix Data object
 * @return void* Shared Memory Matrix Data object
 */
void *getSharedMemoryMatrixData(const Matrix matrix);

/**
 * @brief Execute the process gracefully
 *
 * @param status Exit status
 * @param fd Log file descriptor
 */
void exitGracefully(const int status, const int fd);

/**
 * @brief Write matrix to given path
 *
 * @param path Path to write
 * @param matrix Matrix to write
 * @return int 1 if success, error otherwise
 */
int writeMatrix(const char *path, const Matrix *matrix);

/**
 * @brief Inform user about invalid argument usage
 *
 */
void invalid_usage();

#endif