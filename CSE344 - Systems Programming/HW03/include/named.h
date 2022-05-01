#ifndef NAMED_H
#define NAMED_H

#include "common.h"

#define SHARED_MEMORY_NAME "sharedMemoryNamed"

int detectArguments(int argc, char *argv[], char **inputFilePath, char **name);

int runNamed(WholesalerBag wholesalerBag, char **names);

int initializeSemaphores(char **names);

int freeSemaphores(char **names);

#endif
