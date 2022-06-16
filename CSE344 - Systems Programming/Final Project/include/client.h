#ifndef CLIENT_H
#define CLIENT_H

#include "common.h"
Error GLOBAL_ERROR;

typedef struct
{
  char *requestFile;
  int port;
  char *ip;
  List *lines;
  int currentThreadCount;
  int totalThreadCount;
} ClientVariables;

typedef struct
{
  int threadId;
  char line[REQUEST_FILE_LEN];
} ClientThreadHelper;

/**
 * @brief Initialize of the client
 *
 */
int init(int argc, char *argv[]);

/**
 * @brief Send request to server thread func
 *
 * @param arg
 * @return void*
 */
void *sendRequest(void *arg);

/**
 * @brief get ClientPayloadInfo from given line
 * 
 * @param line 
 * @return ClientRequestPayload 
 */
ClientRequestPayload parseLine(char *line);


#endif // CLIENT_H