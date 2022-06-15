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
  freeListExceptData(queue);
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

void test()
{
  Node *tempNode = queue->head;
  printf("in test\n");
  Payload *temp = tempNode->data;
  printf("type: %d\n", temp->type);
  tempNode = tempNode->next;
  temp = tempNode->data;
  printf("type: %d\n", temp->type);
  tempNode = tempNode->next;
  temp = tempNode->data;
  printf("type: %d\n", temp->type);

  printf("queue size: %d\n", queue->size);
}

int init(int argc, char *argv[])
{
  serverVariables.port = 0;
  serverVariables.numberOfThreads = 0;
  Payload payloadServantInit;
  Payload payloadServantResponse;
  Payload payloadClient;
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
  read(newSocket, &payloadServantInit, sizeof(Payload));

  if (payloadServantInit.type == SERVANT_INIT)
  {
    dprintf(STDOUT_FILENO, "%s: Servant %d present at port %d handling cities %s-%s\n", getTime(), payloadServantInit.servantInitPayload.pid, payloadServantInit.servantInitPayload.port, payloadServantInit.servantInitPayload.startCityName, payloadServantInit.servantInitPayload.endCityName);
  }

  close(networkSocket);

  int port = payloadServantInit.servantInitPayload.port;
  char *ip = payloadServantInit.servantInitPayload.ip;

  payloadClient.type = CLIENT;
  strcpy(payloadClient.clientRequestPayload.startDate, "01-01-2073");
  strcpy(payloadClient.clientRequestPayload.endDate, "30-12-2074");
  strcpy(payloadClient.clientRequestPayload.transactionType, "TARLA");
  strcpy(payloadClient.clientRequestPayload.cityName, "ADANA");
  strcpy(payloadClient.clientRequestPayload.requestType, "transactionCount");

  if ((networkSocket = sendInfoToSocket(payloadClient, port, ip)) < 0)
  {
    printError(STDERR_FILENO, SOCKET_ERROR);
    return -1;
  }
  read(networkSocket, &payloadServantResponse, sizeof(Payload));
  dprintf(STDOUT_FILENO, "%s: type %d returned res: %d\n", getTime(), payloadServantResponse.type, payloadServantResponse.servantResponsePayload.numberOfTransactions);

  close(networkSocket);

  addNode(queue, &payloadServantInit);
  addNode(queue, &payloadClient);
  addNode(queue, &payloadServantResponse);

  test();

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
