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
  int ownPort;
  int cityStart;
  int cityEnd;
  List *cities;
  List *citiesStruct;
  int totalRequestHandled;
} ServantVariables;

typedef struct
{
  int id;
  char *realEstateType;
  char *streetName;
  int surfaceArea;
  int price;
  int day;
  int month;
  int year;
} Record;

typedef struct
{
  char *name;
  List *records;
} City;

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

int getCityInformation(char *cityName);

/**
 * @brief Create a Record object
 *
 * @param line
 * @return Record*
 */
Record *createRecord(char *line);

/**
 * @brief Print the usage in wrong usage
 *
 */
void printUsage();

void printCity(void *city);

void printRecord(void *record);

/**
 * @brief Free the given record
 *
 * @param record record to be freed
 */
void freeRecord(void *record);

/**
 * @brief Free the given city
 *
 * @param city city to be freed
 */
void freeCity(void *city);

#endif // SERVANT_H