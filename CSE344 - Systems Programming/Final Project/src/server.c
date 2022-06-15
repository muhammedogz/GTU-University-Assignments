#include <stdio.h>
#include "../include/server.h"

ServerVariables serverVariables;
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

int detectArguments(int argc, char *argv[])
{
  if (argc != 5)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int pFound = 0;
  int tFound = 0;
  while ((opt = getopt(argc, argv, "p:t:")) != -1)
  {
    switch (opt)
    {
    case 'p':
      if (pFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      serverVariables.port = atoi(optarg);
      pFound = 1;
      break;
    case 't':
      if (tFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      serverVariables.numberOfThreads = atoi(optarg);
      tFound = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!tFound || !pFound)
  {
    dprintf(STDERR_FILENO, "[!] Invalid arguments.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if port is bigger than 2000
  if (serverVariables.port < 2000)
  {
    dprintf(STDERR_FILENO, "[!] Port must be bigger than 2000.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // print all
  dprintf(STDOUT_FILENO, "%s: PORT: %d\n", getTime(), serverVariables.port);
  dprintf(STDOUT_FILENO, "%s: NumThreads: %d\n", getTime(), serverVariables.numberOfThreads);

  return 1;
}

int init(int argc, char *argv[])
{
  serverVariables.port = 0;
  serverVariables.numberOfThreads = 0;

  if (detectArguments(argc, argv) != 0)
  {
    printUsage();
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Server is initializing \n", getTime());
  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  while (!sigintReceived)
  {
    sleep(1);
    printf("domates\n");
  }

  return 0;
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: ./server -p <port> -t <numberOfThreads>\n");
}
