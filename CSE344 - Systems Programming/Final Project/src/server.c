#include <stdio.h>
#include "../include/server.h"

ServerVariables serverVariables;
List *queue = NULL;
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

  return 0;
}

int init(int argc, char *argv[])
{
  serverVariables.port = 0;
  serverVariables.numberOfThreads = 0;
  Payload payload;
  queue = initializeList();
  int networkSocket = 0;
  int newSocket = 0;

  if (detectArguments(argc, argv) != 0)
  {
    printUsage();
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  if ((networkSocket = initializeSocket(serverVariables.port)) < 0)
  {
    printError(STDERR_FILENO, SOCKET_ERROR);
    return -1;
  }

  if ((newSocket = accept(networkSocket, NULL, NULL)) < 0)
  {
    GLOBAL_ERROR = ACCEPT_ERROR;
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }
  read(newSocket, &payload, sizeof(Payload));

  if (payload.type == SERVANT_INIT)
  {
    dprintf(STDOUT_FILENO, "%s: Servant %d present at port %d handling cities %s-%s\n", getTime(), payload.servantInitPayload.pid, payload.servantInitPayload.port, payload.servantInitPayload.startCityName, payload.servantInitPayload.endCityName);
  }

  close(networkSocket);

  int port = payload.servantInitPayload.port;
  addNode(queue, &payload);

  payload.type = CLIENT;
  strcpy(payload.clientRequestPayload.startDate, "01-01-2073");
  strcpy(payload.clientRequestPayload.endDate, "30-12-2074");
  strcpy(payload.clientRequestPayload.transactionType, "TARLA");
  strcpy(payload.clientRequestPayload.cityName, "ADANA");
  strcpy(payload.clientRequestPayload.requestType, "transactionCount");

  if ((networkSocket = sendInfoToSocket(payload, port)) < 0)
  {
    printError(STDERR_FILENO, SOCKET_ERROR);
    return -1;
  }

  read(networkSocket, &payload, sizeof(Payload));
  dprintf(STDOUT_FILENO, "%s: type %d returned res: %d\n", getTime(), payload.type, payload.servantResponsePayload.numberOfTransactions);

  close(networkSocket);

  Payload *temp = queue->head->data;
  printf("type: %d\n", temp->type);

  // while (!sigintReceived)
  // {
  //   sleep(1);
  //   printf("domates\n");
  // }

  return 0;
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: ./server -p <port> -t <numberOfThreads>\n");
}
