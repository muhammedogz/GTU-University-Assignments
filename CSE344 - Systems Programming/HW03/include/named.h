#ifndef NAMED_H
#define NAMED_H

#include "common.h"

int detectArguments(int argc, char *argv[], char **inputFilePath, char **name);

int runNamed(WholesalerBag wholesalerBag, char **names);

int initializeSemaphores(char **names);

int freeSemaphores(char **names);

int pusherW();
int pusherS();
int pusherF();
int pusherM();

void chefWS();
void chefFW();
void chefSF();
void chefMF();
void chefMW();
void chefSM();

#endif
