#include <stdio.h>
#include "../include/server.h"

static pthread_mutex_t handleRequestMutex, servantKeeperMutex;
static pthread_cond_t handleRequestCondition;
ServerVariables serverVariables;

static Payload servents[1000];
static int serventCount = 0;
static int sigintReceived = 0;
char ip[IP_LEN];

typedef struct PayloadQueue
{
  Payload payload;
  struct PayloadQueue *next;
} PayloadQueue;

void addQueue(PayloadQueue **head, Payload payload)
{
  PayloadQueue *node = (PayloadQueue *)malloc(sizeof(PayloadQueue));
  node->payload = payload;
  node->next = NULL;
  if (*head == NULL)
  {
    *head = node;
  }
  else
  {
    PayloadQueue *current = *head;
    while (current->next != NULL)
    {
      current = current->next;
    }
    current->next = node;
  }
}

Payload removeQueue(PayloadQueue **head)
{
  PayloadQueue *node = *head;
  Payload payload = node->payload;
  *head = node->next;
  free(node);
  return payload;
}

PayloadQueue *head = NULL;

void signalHandler()
{
  sigintReceived = 1;
  printf("[!] SIGINT received.\n");
}

void atexitHandler()
{
  dprintf(STDERR_FILENO, "[!] Exiting Server...\n");
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
  // dprintf(STDOUT_FILENO, "%s: PORT: %d\n", getTime(), serverVariables.port);
  // dprintf(STDOUT_FILENO, "%s: NumThreads: %d\n", getTime(), serverVariables.numberOfThreads);

  return 0;
}

int init(int argc, char *argv[])
{
  serverVariables.port = 0;
  serverVariables.numberOfThreads = 0;
  int networkSocket = 0;
  int newSocket = 0;
  pthread_mutex_init(&handleRequestMutex, NULL);
  pthread_mutex_init(&servantKeeperMutex, NULL);
  pthread_cond_init(&handleRequestCondition, NULL);

  if (detectArguments(argc, argv) != 0)
  {
    printUsage();
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }

  int threadCount = serverVariables.numberOfThreads;
  pthread_t handleRequestThreads[threadCount];
  for (int i = 0; i < threadCount; i++)
  {
    if (pthread_create(&handleRequestThreads[i], NULL, handleRequest, NULL) != 0)
    {
      dprintf(STDERR_FILENO, "[!] Error creating thread.\n");
      return -1;
    }
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

  struct sockaddr_in server_address;
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(serverVariables.port);
  server_address.sin_addr.s_addr = INADDR_ANY;
  int addressSize = sizeof(server_address);

  while (1)
  {
    Payload payload;
    if ((newSocket = accept(networkSocket, (struct sockaddr *)&server_address, (socklen_t *)&addressSize)) < 0)
    {

      GLOBAL_ERROR = ACCEPT_ERROR;
      printError(STDERR_FILENO, GLOBAL_ERROR);
      return -1;
    }
    strcpy(ip, payload.ip);
    read(newSocket, &payload, sizeof(Payload));
    pthread_mutex_lock(&handleRequestMutex);
    payload.clientRequestPayload.socketFd = newSocket;
    addQueue(&head, payload);
    pthread_cond_signal(&handleRequestCondition);
    pthread_mutex_unlock(&handleRequestMutex);
  }

  // join thread
  for (int i = 0; i < threadCount; i++)
  {
    pthread_join(handleRequestThreads[i], NULL);
  }

  // close(networkSocket);

  return 0;
}

void *handleRequest()
{

  while (1)
  {
    pthread_mutex_lock(&handleRequestMutex);
    while (head == NULL)
    {
      // if (sigintReceived == 1)
      // {
      //   pthread_mutex_unlock(&handleRequestMutex);
      //   return NULL;
      // }

      pthread_cond_wait(&handleRequestCondition, &handleRequestMutex);
    }
    Payload payload = removeQueue(&head);

    pthread_mutex_unlock(&handleRequestMutex);

    if (sigintReceived)
    {
      printf("**********sasasaa**************\n");
      payload.type = SIGINT_RECEIVED;
      // send all servants
      for (int i = 0; i < serventCount; i++)
      {
        if (sendInfoToSocket(payload, servents[i].servantInitPayload.port, ip) < 0)
        {
          printError(STDERR_FILENO, SOCKET_ERROR);
          return NULL;
        }
      }
    }

    if (payload.type == SERVANT_INIT)
    {
      dprintf(STDOUT_FILENO, "%s: Servant %d present at port %d handling cities %s-%s\n", getTime(), payload.servantInitPayload.pid, payload.servantInitPayload.port, payload.servantInitPayload.startCityName, payload.servantInitPayload.endCityName);

      pthread_mutex_lock(&servantKeeperMutex);
      servents[serventCount++] = payload;

      pthread_mutex_unlock(&servantKeeperMutex);
    }
    else if (payload.type == CLIENT)
    {
      int clientSocket = payload.clientRequestPayload.socketFd;
      Payload payloadServantResponse;
      payloadServantResponse.servantResponsePayload.numberOfTransactions = 0;
      // int totalReq = 0;
      dprintf(STDOUT_FILENO, "%s: Request arrived \"%s\"\n", getTime(), payload.clientRequestPayload.line);
      char *city = payload.clientRequestPayload.cityName;
      if (strcmp(city, NONE_CITY_INFO) == 0)
      {
        int total = 0;

        for (int i = 0; i < serventCount; i++)
        {
          Payload tempPayload = servents[i];

          int port = tempPayload.servantInitPayload.port;
          int networkSocket = 0;
          if ((networkSocket = sendInfoToSocket(payload, port, ip)) < 0)
          {
            printError(STDERR_FILENO, SOCKET_ERROR);
            return NULL;
          }
          read(networkSocket, &payloadServantResponse, sizeof(Payload));
          total += payloadServantResponse.servantResponsePayload.numberOfTransactions;
          // close(networkSocket);
        }
        payloadServantResponse.servantResponsePayload.numberOfTransactions = total;
        write(clientSocket, &payloadServantResponse, sizeof(Payload));
      }

      for (int i = 0; i < serventCount; i++)
      {
        Payload tempPayload = servents[i];
        char *tempStartCity = tempPayload.servantInitPayload.startCityName;
        char *tempEndCity = tempPayload.servantInitPayload.endCityName;

        if (strcmp(city, tempStartCity) >= 0 && strcmp(city, tempEndCity) <= 0)
        {
          int port = tempPayload.servantInitPayload.port;
          int networkSocket = 0;

          if ((networkSocket = sendInfoToSocket(payload, port, ip)) < 0)
          {
            printError(STDERR_FILENO, SOCKET_ERROR);
            return NULL;
          }
          read(networkSocket, &payloadServantResponse, sizeof(Payload));

          write(clientSocket, &payloadServantResponse, sizeof(Payload));
          // close(networkSocket);
        }
      }
    }

    close(payload.clientRequestPayload.socketFd);
  }
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: ./server -p <port> -t <numberOfThreads>\n");
}
