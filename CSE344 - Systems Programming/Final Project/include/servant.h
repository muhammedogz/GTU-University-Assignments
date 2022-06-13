#ifndef SERVANT_H
#define SERVANT_H

#include "common.h"
Error GLOBAL_ERROR;

typedef struct {
  char *directoryPath;
char *cities;
char *ipAddress;
int port;
  int cityStart;
  int cityEnd;
} ArgumentInfo;


/**
 * @brief Initialize of the servant
 * 
 */
void init(void);

/**
 * @brief 
 * 
 * @param argc 
 * @param argv 
 * @return int 
 */
int detectArguments(int argc, char *argv[]);

void printUsage();

#endif // SERVANT_H