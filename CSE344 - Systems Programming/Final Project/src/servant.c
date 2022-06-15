#include <stdio.h>
#include "../include/servant.h"

ServantVariables servantVariables;

int sigintReceived = 0;

void signalHandler()
{
  sigintReceived = 1;
  printf("\n");
  printf("[!] SIGINT received.\n");
}

void atexitHandler()
{
  freeList(servantVariables.cities, freeString);
  freeList(servantVariables.citiesStruct, freeCity);
}

int detectArguments(int argc, char *argv[])
{
  if (argc != 9)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int dFound = 0;
  int cFound = 0;
  int rFound = 0;
  int pFound = 0;
  while ((opt = getopt(argc, argv, "d:c:r:p:")) != -1)
  {
    switch (opt)
    {
    case 'd':
      if (dFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.directoryPath = optarg;
      dFound = 1;
      break;
    case 'c':
      if (cFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.cityInterval = optarg;
      cFound = 1;
      break;
    case 'r':
      if (rFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.ipAddress = optarg;
      rFound = 1;
      break;
    case 'p':
      if (pFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.port = atoi(optarg);
      pFound = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!dFound || !cFound || !rFound || !pFound)
  {
    dprintf(STDERR_FILENO, "[!] Invalid arguments.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // divide cities to two parts with delimeter -
  char *p = servantVariables.cityInterval;
  char *q = strchr(p, '-');
  if (q == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Invalid cities.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }
  *q = '\0';
  servantVariables.cityStart = atoi(p);
  servantVariables.cityEnd = atoi(q + 1);
  if (servantVariables.cityStart > servantVariables.cityEnd)
  {
    dprintf(STDERR_FILENO, "[!] City start must be less than city end.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if port is bigger than 2000
  if (servantVariables.port < 2000)
  {
    dprintf(STDERR_FILENO, "[!] Port must be bigger than 2000.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if given directory exist
  if (access(servantVariables.directoryPath, F_OK) == -1)
  {
    dprintf(STDERR_FILENO, "[!] Directory %s does not exist.\n", servantVariables.directoryPath);
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  List *list = NULL;
  if ((list = initializeList()) == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot initialize list.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  DIR *directory;
  int directoryCount = 0;
  struct dirent *dir;
  directory = opendir(servantVariables.directoryPath);
  if (directory)
  {
    while ((dir = readdir(directory)) != NULL)
    {
      if (strcmp(dir->d_name, ".") == 0 || strcmp(dir->d_name, "..") == 0)
      {
        continue;
      }

      directoryCount++;
      char *temp = malloc(sizeof(char) * (strlen(dir->d_name) + 1));
      strcpy(temp, dir->d_name);
      addNode(list, temp);

      // printf("%s\n", dir->d_name);
    }
    closedir(directory);
  }
  sortList(list, compareString);

  // check if directory count is equal to start and end city
  if (directoryCount - 2 < servantVariables.cityEnd - servantVariables.cityStart + 1)
  {
    dprintf(STDERR_FILENO, "[!] Directory count is not equal to start and end city.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  servantVariables.cities = NULL;
  if ((servantVariables.cities = getListInRangeString(list, servantVariables.cityStart - 1, servantVariables.cityEnd - 1)) == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot get cities.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }
  freeList(list, freeString);
  // printList(servantVariables.cities, printString);

  // print all
  // dprintf(STDOUT_FILENO, "%s: Input file 1: %s\n", getTime(), argumentInfo.directoryPath);
  // dprintf(STDOUT_FILENO, "%s: Input file 2: %s\n", getTime(), argumentInfo.cities);
  // dprintf(STDOUT_FILENO, "%s: IP address: %s\n", getTime(), argumentInfo.ipAddress);
  // dprintf(STDOUT_FILENO, "%s: Port: %d\n", getTime(), argumentInfo.port);
  // dprintf(STDOUT_FILENO, "%s: City start: %d\n", getTime(), argumentInfo.cityStart);
  // dprintf(STDOUT_FILENO, "%s: City end: %d\n", getTime(), argumentInfo.cityEnd);
  // dprintf(STDOUT_FILENO, "%s: Directory count: %d\n", getTime(), directoryCount);

  return 0;
}

int init(int argc, char *argv[])
{
  servantVariables.pid = getOwnPid();
  servantVariables.cities = NULL;
  servantVariables.citiesStruct = NULL;
  servantVariables.directoryPath = NULL;
  servantVariables.ipAddress = NULL;
  servantVariables.port = 0;
  servantVariables.ownPort = 0;
  servantVariables.cityStart = 0;
  servantVariables.cityEnd = 0;
  servantVariables.totalRequestHandled = 0;
  servantVariables.cityInterval = NULL;
  servantVariables.serverOwnSocket = 0;
  int servantOwnSocket = 0;
  int servantInitializationSocket = 0;
  int clientResponseSocket = 0;

  if (detectArguments(argc, argv) != 0)
  {
    printUsage();
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }
  servantVariables.ownPort = servantVariables.port + servantVariables.cityStart;

  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }

  // get all information from files
  servantVariables.citiesStruct = initializeList();
  Node *tempNode = servantVariables.cities->head;
  while (tempNode != NULL)
  {
    getCityInformation(tempNode->data);
    tempNode = tempNode->next;
  }

  dprintf(STDOUT_FILENO, "%s: Servant %d: loaded dataset. cities %s-%s\n", getTime(), servantVariables.pid, (char *)listGetFirst(servantVariables.cities), (char *)listGetLast(servantVariables.cities));
  dprintf(STDOUT_FILENO, "%s: Servant %d: listenint at port %d\n", getTime(), servantVariables.pid, servantVariables.ownPort);

  Payload payload;
  payload.type = SERVANT_INIT;
  char *startCityName = listGetFirst(servantVariables.cities);
  char *endCityName = listGetLast(servantVariables.cities);

  payload.servantInitPayload.port = servantVariables.ownPort;
  payload.servantInitPayload.pid = servantVariables.pid;
  strcpy(payload.servantInitPayload.ip, servantVariables.ipAddress);
  strcpy(payload.servantInitPayload.startCityName, startCityName);
  strcpy(payload.servantInitPayload.endCityName, endCityName);
  payload.servantInitPayload.startCityIndex = servantVariables.cityStart;
  payload.servantInitPayload.endCityIndex = servantVariables.cityEnd;

  if ((servantInitializationSocket = sendInfoToSocket(payload, servantVariables.port, servantVariables.ipAddress)) < 0)
  {

    printError(STDERR_FILENO, SOCKET_ERROR);
  }
  close(servantInitializationSocket);

  if ((servantOwnSocket = initializeSocket(servantVariables.ownPort)) < 0)
  {
    printError(STDERR_FILENO, SOCKET_ERROR);
    return -1;
  }
  servantVariables.serverOwnSocket = servantOwnSocket;

  // struct sockaddr_in server_address;
  // server_address.sin_family = AF_INET;
  // server_address.sin_port = htons(servantVariables.ownPort);
  // server_address.sin_addr.s_addr = INADDR_ANY;
  // int addressSize = sizeof(server_address);
  // if ((clientResponseSocket = accept(servantOwnSocket, (struct sockaddr *)&server_address, (socklen_t *)&addressSize)) < 0)
  while (1)
  {
    printf("Waiting for client...\n");
    if ((clientResponseSocket = accept(servantOwnSocket, NULL, NULL)) < 0)
    {
      GLOBAL_ERROR = ACCEPT_ERROR;
      printError(STDERR_FILENO, GLOBAL_ERROR);
      return -1;
    }
    read(clientResponseSocket, &payload, sizeof(Payload));

    if (payload.type == CLIENT)
    {
      dprintf(STDOUT_FILENO, "%s: Servant %d: received request from client city name %s\n", getTime(), servantVariables.pid, payload.clientRequestPayload.cityName);
      dprintf(STDOUT_FILENO, "%s: Servant %d: start date %s\n", getTime(), servantVariables.pid, payload.clientRequestPayload.startDate);
      dprintf(STDOUT_FILENO, "%s: Servant %d: end date %s\n", getTime(), servantVariables.pid, payload.clientRequestPayload.endDate);
      dprintf(STDOUT_FILENO, "%s: Servant %d: request name %s\n", getTime(), servantVariables.pid, payload.clientRequestPayload.requestType);
      dprintf(STDOUT_FILENO, "%s: Servant %d: transaction type %s\n", getTime(), servantVariables.pid, payload.clientRequestPayload.transactionType);
      int res = findTransactionCount(payload.clientRequestPayload.startDate, payload.clientRequestPayload.endDate, payload.clientRequestPayload.transactionType, payload.clientRequestPayload.cityName);
      dprintf(STDOUT_FILENO, "%s: Servant %d: found %d\n", getTime(), servantVariables.pid, res);

      Payload response;
      response.type = SERVANT_RESPONSE;
      response.servantResponsePayload.numberOfTransactions = res;
      printf("Response type: %d\n", response.type);
      printf("Response number of transactions: %d\n", response.servantResponsePayload.numberOfTransactions);
      write(clientResponseSocket, &response, sizeof(Payload));
    }
    else if (payload.type == SIGINT_RECEIVED)
    {
      dprintf(STDOUT_FILENO, "%s: Servant %d: received SIGINT\n", getTime(), servantVariables.pid);
      break;
    }
    else
    {
      dprintf(STDOUT_FILENO, "%s: Servant %d: received invalid payload type. Expecting 2, Get %d\n", getTime(), servantVariables.pid, payload.type);
      printError(STDERR_FILENO, INVALID_RESPONSE_TYPE);
    }
    close(clientResponseSocket);
  }

  close(servantOwnSocket);

  dprintf(STDOUT_FILENO, "%s: Servant %d: finished.\n", getTime(), servantVariables.pid);
  // while (!sigintReceived)
  //   ;

  return 0;
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: ./servant -d <directory path> -c <cities file> -r <ip address> -p <port>\n");
}

int findTransactionCount(char *startDate, char *endDate, char *type, char *cityName)
{
  int isExist = strcmp(cityName, NONE_CITY_INFO) == 0 ? 1 : 0;
  if (strcmp(cityName, NONE_CITY_INFO) != 0)
  {
    // search list if the given cityName exit in the list
    Node *tempNode = servantVariables.citiesStruct->head;
    while (tempNode != NULL)
    {
      City *tempCity = tempNode->data;

      if (strcmp(tempCity->name, cityName) == 0)
      {
        isExist = 1;
        break;
      }
      tempNode = tempNode->next;
    }
  }

  if (isExist == 0)
  {
    return -1;
  }

  int transactionCount = 0;
  int startDateDay, startDateMonth, startDateYear;
  int endDateDay, endDateMonth, endDateYear;

  if (sscanf(startDate, "%d-%d-%d", &startDateDay, &startDateMonth, &startDateYear) != 3)
  {
    dprintf(STDERR_FILENO, "[!] Cannot parse start date.\n");
    return -1;
  }
  if (sscanf(endDate, "%d-%d-%d", &endDateDay, &endDateMonth, &endDateYear) != 3)
  {
    dprintf(STDERR_FILENO, "[!] Cannot parse end date.\n");
    return -1;
  }

  int fullStartDate = startDateYear * 365 + startDateMonth * 12 + startDateDay;
  int fullEndDate = endDateYear * 365 + endDateMonth * 12 + endDateDay;

  Node *tempCityStructNode = servantVariables.citiesStruct->head;
  while (tempCityStructNode != NULL)
  {
    City *tempCity = (City *)tempCityStructNode->data;
    Node *tempRecordNode = tempCity->records->head;
    while (tempRecordNode != NULL)
    {
      Record *tempRecord = (Record *)tempRecordNode->data;
      tempRecordNode = tempRecordNode->next;
      if (strcmp(tempRecord->realEstateType, type) != 0)
        continue;

      int recordFullTime = tempRecord->year * 365 + tempRecord->month * 12 + tempRecord->day;

      if (recordFullTime >= fullStartDate && recordFullTime <= fullEndDate)
      {

        if (strcmp(cityName, NONE_CITY_INFO) == 0)
          transactionCount++;
        else if (strcmp(tempCity->name, cityName) == 0)
          transactionCount++;
      }
    }

    tempCityStructNode = tempCityStructNode->next;
  }

  return transactionCount;
}

Record *createRecord(char *line)
{
  Record *record = malloc(sizeof(Record));
  record->id = 0;
  record->price = 0;
  record->surfaceArea = 0;
  record->day = 0;
  record->month = 0;
  record->year = 0;
  record->streetName = NULL;
  record->realEstateType = NULL;
  if (record == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot allocate memory for record.\n");
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  // split string by space
  char *token = strtok(line, " ");
  if (token == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot split string.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return NULL;
  }

  int i = 0;
  while (token != NULL)
  {
    switch (i)
    {
    case 0:
      record->id = atoi(token);
      break;
    case 1:
      record->realEstateType = malloc(sizeof(char) * (strlen(token) + 1));
      strcpy(record->realEstateType, token);
      break;
    case 2:
      record->streetName = malloc(sizeof(char) * (strlen(token) + 1));
      strcpy(record->streetName, token);
      break;
    case 3:
      record->surfaceArea = atoi(token);
      break;
    case 4:
      record->price = atoi(token);
      break;
    }
    token = strtok(NULL, " ");
    i++;
  }

  return record;
}

int getCityInformation(char *cityName)
{
  City *city = malloc(sizeof(City));
  city->name = malloc(sizeof(char) * (strlen(cityName) + 1));
  strcpy(city->name, cityName);
  city->records = initializeList();
  addNode(servantVariables.citiesStruct, city);

  char directoryPath[100];
  sprintf(directoryPath, "%s/%s", servantVariables.directoryPath, cityName);

  DIR *dir = opendir(directoryPath);
  if (dir == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot open directory.\n");
    return -1;
  }

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL)
  {
    int day, month, year;
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
      continue;
    char filePath[400];
    snprintf(filePath, 400, "%s/%s", directoryPath, entry->d_name);
    if (sscanf(entry->d_name, "%d-%d-%d", &day, &month, &year) != 3)
    {
      dprintf(STDERR_FILENO, "[!] Cannot parse file name.\n");
      return -1;
    }

    int fd = open(filePath, O_RDONLY, 666);
    if (fd == -1)
    {
      dprintf(STDERR_FILENO, "[!] Cannot open file.\n");
      return -1;
    }

    // read whole file
    int fileSize = getFileSize(filePath);
    char *buffer = malloc(sizeof(char) * fileSize + 1);
    int bytesRead = read(fd, buffer, fileSize);
    if (bytesRead == -1)
    {
      dprintf(STDERR_FILENO, "[!] Cannot read file.\n");
      return -1;
    }
    buffer[bytesRead] = '\0';
    close(fd);

    char *line = strtok(buffer, "\n");
    List *lines = initializeList();
    while (line != NULL)
    {
      char *tempStr = malloc(sizeof(char) * (strlen(line) + 1));
      strcpy(tempStr, line);
      addNode(lines, tempStr);
      line = strtok(NULL, "\n");
    }

    Node *tempLineNode = lines->head;

    while (tempLineNode != NULL)
    {
      char *line = tempLineNode->data;
      Record *record = createRecord(line);
      if (record == NULL)
      {
        dprintf(STDERR_FILENO, "[!] Cannot create record.\n");
        return -1;
      }
      record->day = day;
      record->month = month;
      record->year = year;
      addNode(city->records, record);
      tempLineNode = tempLineNode->next;
    }

    free(buffer);
    freeList(lines, freeString);
  }

  closedir(dir);

  return 0;
}

void freeRecord(void *record)
{
  Record *recordPtr = (Record *)record;
  free(recordPtr->realEstateType);
  free(recordPtr->streetName);
  free(recordPtr);
}

void freeCity(void *city)
{
  City *cityPtr = (City *)city;
  free(cityPtr->name);
  freeList(cityPtr->records, freeRecord);
  free(cityPtr);
}

void printCity(void *city)
{
  City *cityPtr = (City *)city;
  dprintf(STDOUT_FILENO, "-----------------------------------------------\n");
  dprintf(STDOUT_FILENO, "%s: Servant %d: city %s\n", getTime(), servantVariables.pid, cityPtr->name);
  printList(cityPtr->records, printRecord);
  dprintf(STDOUT_FILENO, "-----------------------------------------------\n");
}

void printRecord(void *record)
{
  Record *recordPtr = (Record *)record;
  dprintf(STDOUT_FILENO, "%s: Servant %d: Record id:%d\n", getTime(), servantVariables.pid, recordPtr->id);
  dprintf(STDOUT_FILENO, "%s: Servant %d: Record type:%s\n", getTime(), servantVariables.pid, recordPtr->realEstateType);
  dprintf(STDOUT_FILENO, "%s: Servant %d: Record street name:%s\n", getTime(), servantVariables.pid, recordPtr->streetName);
  dprintf(STDOUT_FILENO, "%s: Servant %d: Record surface area:%d\n", getTime(), servantVariables.pid, recordPtr->surfaceArea);
  dprintf(STDOUT_FILENO, "%s: Servant %d: Record price:%d\n", getTime(), servantVariables.pid, recordPtr->price);
  dprintf(STDOUT_FILENO, "%s: Servant %d: Record date: %d-%d-%d\n", getTime(), servantVariables.pid, recordPtr->day, recordPtr->month, recordPtr->year);
}