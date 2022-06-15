#ifndef COMMON_H
#define COMMON_H

#define BUFFER_SIZE 5000
#define PROC_STAT_PATH "/proc/self/stat"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <pthread.h>
#include <dirent.h>
#include <arpa/inet.h>
#include <sys/procfs.h>
#include <sys/syscall.h>
#include "list.h"

#define BACKLOG_COUNT 25
#define CITY_NAME_LEN 50
#define IP_LEN 20
#define REQUEST_TYPE_LEN 50
#define TRANSACTION_LEN 30
#define DATE_LEN 12

/**
 * @brief Error codes
 */
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
  FILE_UNLINK_ERROR,
  INVALID_ARGUMENTS,
  INVALID_EXECVE,
  INVALID_FORK,
  INVALID_WAIT,
  INVALID_MATRIX,
  ALREADY_RUNNING,
  PIPE_CREATION_ERROR,
  FORK_ERROR,
  PIPE_READ_ERROR,
  PIPE_CLOSE_ERROR,
  PIPE_WRITE_ERROR,
  PRINT_ERROR,
  FIRST_INITIALIZE_SERVER,
  FILE_TRUNCATE_ERROR,
  FILE_MMAP_ERROR,
  UNLINK_ERROR,
  SEMAPHORE_OPEN_ERROR,
  SEMAPHORE_UNLINK_ERROR,
  SEMAPHORE_CLOSE_ERROR,
  WAITPID_ERROR,
  SEMAPHORE_INIT_FAILED,
  INVALID_THREAD_CREATION,
  INVALID_THREAD_DETACH,
  INVALID_THREAD_JOIN,
  SEMAPHORE_GET_ERROR,
  SEMAPHORE_OPERATION_FAILED,
  MUTEX_INIT_ERROR,
  COND_INIT_ERROR,
  COND_BROADCAST_ERROR,
  COND_WAIT_ERROR,

  //
  SIGACTION_FAILURE,
  ATEXIT_FAILURE,
  SOCKET_ERROR,
  BIND_ERROR,
  LISTEN_ERROR,
  ACCEPT_ERROR,
  CONNECT_ERROR,
  INVALID_RESPONSE_TYPE,
  INVALID_IP,

  //
  INVALID_EXIT_STATUS,
} Error;

typedef enum
{
  SERVANT_INIT,
  SERVANT_RESPONSE,
  CLIENT,
} RequestType;

typedef struct
{
  int port;
  pid_t pid;
  char ip[IP_LEN];
  char startCityName[CITY_NAME_LEN];
  char endCityName[CITY_NAME_LEN];
  int startCityIndex;
  int endCityIndex;
} ServantInitPayload;

typedef struct
{
  int numberOfTransactions;
} ServantResponsePayload;

typedef struct
{
  char requestType[REQUEST_TYPE_LEN];
  char transactionType[TRANSACTION_LEN];
  char startDate[DATE_LEN];
  char endDate[DATE_LEN];
  char cityName[CITY_NAME_LEN];

} ClientRequestPayload;

typedef struct
{
  RequestType type;
  ServantInitPayload servantInitPayload;
  ServantResponsePayload servantResponsePayload;
  ClientRequestPayload clientRequestPayload;
} Payload;

/**
 * @brief Initialize a singal handler and assign a atexit function
 *
 * @param signalType Signal type
 * @param signalHandlerFunction Signal handler function
 * @param atexitFunction atexit function
 * @return int 0 on success, others on failure
 */
int initializeSignalAndAtexit(int signalType, void *signalHandlerFunction, void *atexitFunction);

/**
 * @brief Initialize a socket on given port
 *
 * @param port Port number
 * @return int Socket file descriptor on success, others on failure
 */
int initializeSocket(int port);

/**
 * @brief Send given info to given port
 *
 * @param payload Payload to send
 * @param port Port to send to
 * @return int
 */
int sendInfoToSocket(Payload payload, int port, char *ip);

/**
 * @brief Print error and exit
 *
 * @param fd file descriptor to print error
 */
void printError(const int fd, const Error error);

/**
 * @brief Get the File Size object
 *
 * @param filename
 * @return off_t
 */
off_t getFileSize(const char *filename);

/**
 * @brief Get the Time object
 *
 * @return char*
 */
char *getTime();

/**
 * @brief Get the Own Pid object
 *
 * @return pid_t
 */
pid_t getOwnPid();

#endif // COMMON_H