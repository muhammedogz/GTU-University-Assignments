#include <stdio.h>
#include "../include/client.h"

static pthread_mutex_t handleRequestMutex;
static pthread_cond_t handleRequestCondition;
static int sigintReceived = 0;
static ClientVariables clientVariables;

void signalHandler()
{
  sigintReceived = 1;
  printf("\n");
  printf("[!] SIGINT received.\n");
}

void atexitHandler()
{
  freeList(clientVariables.lines, freeString);
}

int detectArguments(int argc, char *argv[])
{
  if (argc != 7)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int rFound = 0;
  int qFound = 0;
  int sFound = 0;
  while ((opt = getopt(argc, argv, "r:q:s:")) != -1)
  {
    switch (opt)
    {
    case 'r':
      if (rFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      clientVariables.requestFile = optarg;
      rFound = 1;
      break;
    case 'q':
      if (qFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      clientVariables.port = atoi(optarg);
      qFound = 1;
      break;
    case 's':
      if (sFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      clientVariables.ip = optarg;
      sFound = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!rFound || !qFound || !sFound)
  {
    dprintf(STDERR_FILENO, "[!] Invalid arguments.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if port is bigger than 2000
  if (clientVariables.port < 2000)
  {
    dprintf(STDERR_FILENO, "[!] Port must be bigger than 2000.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // print all
  // dprintf(STDOUT_FILENO, "%s: PORT: %d\n", getTime(), clientVariables.port);
  // dprintf(STDOUT_FILENO, "%s: ip: %s\n", getTime(), clientVariables.ip);
  // dprintf(STDOUT_FILENO, "%s: requestFile: %s\n", getTime(), clientVariables.requestFile);

  return 0;
}

void readRequestFile()
{
  int fd = open(clientVariables.requestFile, O_RDONLY);
  if (fd < 0)
  {
    dprintf(STDERR_FILENO, "[!] Could not open file %s.\n", clientVariables.requestFile);
    return;
  }

  char *buffer = malloc(sizeof(char) * BUFFER_SIZE);
  if (buffer == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Could not allocate memory.\n");
    return;
  }

  int bytesRead = read(fd, buffer, BUFFER_SIZE);
  if (bytesRead < 0)
  {
    dprintf(STDERR_FILENO, "[!] Could not read file %s.\n", clientVariables.requestFile);
    return;
  }

  char *line = strtok(buffer, "\n");
  while (line != NULL)
  {
    char *tempStr = malloc(sizeof(char) * (strlen(line) + 1));
    strcpy(tempStr, line);
    addNode(clientVariables.lines, tempStr);
    line = strtok(NULL, "\n");
  }

  close(fd);
  free(buffer);

  // printList(clientVariables.lines, printString);
}

int init(int argc, char *argv[])
{
  clientVariables.ip = NULL;
  clientVariables.requestFile = NULL;
  clientVariables.lines = initializeList();
  clientVariables.port = 0;
  clientVariables.currentThreadCount = 0;
  clientVariables.totalThreadCount = 0;

  setbuf(stdout, NULL);
  pthread_mutex_init(&handleRequestMutex, NULL);
  pthread_cond_init(&handleRequestCondition, NULL);

  if (detectArguments(argc, argv) != 0)
  {
    dprintf(STDERR_FILENO, "%s: Client is exiting with error %d\n", getTime(), GLOBAL_ERROR);
    return -1;
  }

  dprintf(STDOUT_FILENO, "%s: Client is initializing \n", getTime());
  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }

  readRequestFile();

  int size = clientVariables.lines->size;
  clientVariables.totalThreadCount = size;
  dprintf(STDOUT_FILENO, "%s: Client: I have loaded %d requests and I'm creating %d threads.\n", getTime(), size, size);

  pthread_t threads[size];
  Node *current = clientVariables.lines->head;
  for (int i = 0; i < size; i++)
  {
    ClientThreadHelper *helper = malloc(sizeof(ClientThreadHelper));
    helper->threadId = i;
    strcpy(helper->line, current->data);
    if (pthread_create(&threads[i], NULL, sendRequest, (void *)helper) != 0)
    {
      dprintf(STDERR_FILENO, "%s: Client: Could not create thread %d.\n", getTime(), i);
      return -1;
    }
    current = current->next;
  }

  for (int i = 0; i < size; i++)
  {
    if (pthread_join(threads[i], NULL) != 0)
    {
      dprintf(STDERR_FILENO, "%s: Client: Could not join thread %d.\n", getTime(), i);
      return -1;
    }
  }

  return 0;
}

void *sendRequest(void *arg)
{
  ClientThreadHelper *helper = (ClientThreadHelper *)arg;
  int threadId = helper->threadId;
  char *line = helper->line;
  dprintf(STDOUT_FILENO, "%s: Client-Thread-%d: Thread-%d has been created.\n", getTime(), threadId, threadId);

  pthread_mutex_lock(&handleRequestMutex);
  clientVariables.currentThreadCount++;
  if (clientVariables.currentThreadCount != clientVariables.totalThreadCount)
    pthread_cond_wait(&handleRequestCondition, &handleRequestMutex);
  else
    pthread_cond_broadcast(&handleRequestCondition);
  pthread_mutex_unlock(&handleRequestMutex);

  Payload clientRequest;
  strcpy(clientRequest.ip, clientVariables.ip);
  clientRequest.type = CLIENT;
  clientRequest.clientRequestPayload = parseLine(line);

  dprintf(STDOUT_FILENO, "%s: Client-Thread-%d: I am requesting \"\\%s\"\n", getTime(), threadId, line);

  int network_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (network_socket < 0)
  {
    printError(STDERR_FILENO, SOCKET_ERROR);
    return NULL;
  }
  struct sockaddr_in server_address;
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(clientVariables.port);
  // server_address.sin_addr.s_addr = INADDR_ANY;

  if (inet_pton(AF_INET, clientVariables.ip, &server_address.sin_addr) <= 0)
  {
    dprintf(STDERR_FILENO, "%s: Client: Invalid IP.\n", getTime());
    dprintf(STDERR_FILENO, "%s: Client: %s\n", getTime(), clientVariables.ip);
    printError(STDERR_FILENO, INVALID_IP);
    return NULL;
  }

  if (connect(network_socket, (struct sockaddr *)&server_address, sizeof(server_address)) < 0)
  {
    printError(STDERR_FILENO, CONNECT_ERROR);
    return NULL;
  }
  clientRequest.clientRequestPayload.socketFd = network_socket;

  write(network_socket, &clientRequest, sizeof(Payload));

  Payload response;
  read(network_socket, &response, sizeof(Payload));
  // print response
  dprintf(STDOUT_FILENO, "%s: Client-Thread-%d: The server's response to \"\\%s\" is %d\n", getTime(), threadId, line, response.servantResponsePayload.numberOfTransactions);

  // close(network_socket);
  // free(helper);
  return NULL;
}

ClientRequestPayload parseLine(char *line)
{
  ClientRequestPayload payload;
  int filledCount = sscanf(line, "%s %s %s %s %s", payload.transactionType, payload.requestType, payload.startDate, payload.endDate, payload.cityName);
  if (filledCount == 4)
  {
    strcpy(payload.cityName, NONE_CITY_INFO);
  }
  strcpy(payload.line, line);

  // dprintf(STDOUT_FILENO, "%s: Client: Request: |%s %s %s %s %s|\n", getTime(), payload.transactionType, payload.requestType, payload.startDate, payload.endDate, payload.cityName);
  return payload;
}