#ifndef UNNAMED_H
#define UNNAMED_H

#include <semaphore.h>

typedef struct
{
  // m = Milk
  // f = Flour
  // w = Walnut
  // s = Sugar
  sem_t pusherWorking, chefDelivered, m, f, w, s;
  sem_t ws, fw, sf, mf, mw, sm;
  char ingredient1;
  char ingredient2;
} UnnamedShared;

int detectArguments(int argc, char *argv[], char **inputFilePath);

int runUnNamed(WholesalerBag wholesalerBag);

int initializeSemaphores();

int freeSemaphores();

void *createUnnamedSharedMemory(char *sharedName, char ingredient1, char ingredient2);
void *getUnnamedSharedMemory(char *sharedName);

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
