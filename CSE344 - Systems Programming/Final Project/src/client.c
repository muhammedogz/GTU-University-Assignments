#include <stdio.h>
#include "../include/client.h"

int sigintReceived = 0;

void signalHandler()
{
  sigintReceived = 1;
  printf("\n");
  printf("[!] SIGINT received.\n");
}

void atexitHandler()
{
  printf("[!] Exiting.\n");
}

void init()
{
  dprintf(STDOUT_FILENO, "%s: Client is initializing \n", getTime());
  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }

  while (!sigintReceived)
  {
    sleep(1);
    printf("domates\n");
  }
}