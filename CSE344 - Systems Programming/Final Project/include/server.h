#ifndef SERVER_H
#define SERVER_H

#include "common.h"
Error GLOBAL_ERROR;

typedef struct
{
  int port;
  int numberOfThreads;
} ServerVariables;

/**
 * @brief Initialize of the servant
 *
 */
int init(int argc, char *argv[]);

/**
 * @brief
 *
 * @param argc
 * @param argv
 * @return int
 */
int detectArguments(int argc, char *argv[]);

/**
 * @brief Handle req thread func
 *
 * @return void*
 */
void *handleRequest();

/**
 * @brief print usage info
 *
 */
void printUsage();

#endif // SERVER_H