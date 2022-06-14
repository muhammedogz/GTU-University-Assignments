#include <stdio.h>
#include "../include/servant.h"

ArgumentInfo argumentInfo;

int sigintReceived = 0;

void print(void *data)
{
  printf("%s\n", (char *)data);
}

int compare(void *data1, void *data2)
{
  return strcmp((char *)data1, (char *)data2);
}

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
      argumentInfo.directoryPath = optarg;
      dFound = 1;
      break;
    case 'c':
      if (cFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      argumentInfo.cities = optarg;
      cFound = 1;
      break;
    case 'r':
      if (rFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      argumentInfo.ipAddress = optarg;
      rFound = 1;
      break;
    case 'p':
      if (pFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      argumentInfo.port = atoi(optarg);
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
  char *p = argumentInfo.cities;
  char *q = strchr(p, '-');
  if (q == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Invalid cities.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }
  *q = '\0';
  argumentInfo.cityStart = atoi(p);
  argumentInfo.cityEnd = atoi(q + 1);
  if (argumentInfo.cityStart > argumentInfo.cityEnd)
  {
    dprintf(STDERR_FILENO, "[!] City start must be less than city end.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if port is bigger than 2000
  if (argumentInfo.port < 2000)
  {
    dprintf(STDERR_FILENO, "[!] Port must be bigger than 2000.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if given directory exist
  if (access(argumentInfo.directoryPath, F_OK) == -1)
  {
    dprintf(STDERR_FILENO, "[!] Directory %s does not exist.\n", argumentInfo.directoryPath);
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  List *list = malloc(sizeof(List));

  DIR *d;
  int directoryCount = 0;
  struct dirent *dir;
  d = opendir(argumentInfo.directoryPath);
  if (d)
  {
    while ((dir = readdir(d)) != NULL)
    {
      directoryCount++;
      addNode(list, dir->d_name);
      // printf("%s\n", dir->d_name);
    }
    closedir(d);
  }

  printf("sa\n");

  // print first data in list
  printf("%s\n", (char *)list->head->data);

  printf("as\n");
  printList(list, print);

  sortList(list, compare);
  printf("--------------\n");
  printList(list, print);

  // check if directory count is equal to start and end city
  if (directoryCount - 2 < argumentInfo.cityEnd - argumentInfo.cityStart + 1)
  {
    dprintf(STDERR_FILENO, "[!] Directory count is not equal to start and end city.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // print all
  dprintf(STDOUT_FILENO, "%s: Input file 1: %s\n", getTime(), argumentInfo.directoryPath);
  dprintf(STDOUT_FILENO, "%s: Input file 2: %s\n", getTime(), argumentInfo.cities);
  dprintf(STDOUT_FILENO, "%s: IP address: %s\n", getTime(), argumentInfo.ipAddress);
  dprintf(STDOUT_FILENO, "%s: Port: %d\n", getTime(), argumentInfo.port);
  dprintf(STDOUT_FILENO, "%s: City start: %d\n", getTime(), argumentInfo.cityStart);
  dprintf(STDOUT_FILENO, "%s: City end: %d\n", getTime(), argumentInfo.cityEnd);
  dprintf(STDOUT_FILENO, "%s: Directory count: %d\n", getTime(), directoryCount);

  return 1;
}

void init()
{
  dprintf(STDOUT_FILENO, "%s: Servant is initializing \n", getTime());
  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: ./servant -d <directory path> -c <cities file> -r <ip address> -p <port>\n");
}