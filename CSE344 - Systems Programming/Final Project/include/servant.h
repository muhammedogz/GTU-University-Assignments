#ifndef SERVANT_H
#define SERVANT_H

#include "common.h"
Error GLOBAL_ERROR;

typedef struct
{
  pid_t pid;
  char *directoryPath;
  char *cityInterval;
  char *ipAddress;
  int port;
  int cityStart;
  int cityEnd;
  List *cities;
  int totalRequestHandled;
} ServantVariables;

typedef struct
{
  int id;
  char *realEstateType;
  char *streetName;
  int surfaceArea;
  int price;
} Record;

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

Record *createRecord(char *line);

void printUsage();

#endif // SERVANT_H